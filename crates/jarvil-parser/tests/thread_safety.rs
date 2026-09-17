// Thread safety, enforced by the compiler rather than by inspection.
//
// The static assertions below are the real test: each one fails to compile if
// an `Rc`, `RefCell`, `Cell` or raw pointer reappears anywhere reachable from
// these types. That is a far stronger guarantee than any runtime check, and it
// is what stops the `Send + Sync` property from quietly regressing the next
// time a shared-ownership type is reached for.
//
// The runtime tests then confirm the property is actually usable: that an
// analysis can be produced on one thread and read on another, and that several
// analyses can run concurrently without tripping over shared state.

use jarvil_parser::analysis::{JarvilDiagnostic, diagnostics, with_analysis};
use jarvil_parser::ast::ast::BlockNode;
use jarvil_parser::code::JarvilCode;
use jarvil_parser::error::error::JarvilProgramAnalysisErrors;
use jarvil_parser::scope::semantic_db::SemanticStateDatabase;
use jarvil_parser::types::core::Type;
use std::sync::Arc;
use std::thread;

fn assert_send<T: Send>() {}
fn assert_sync<T: Sync>() {}
fn assert_send_sync<T: Send + Sync>() {}

#[test]
fn core_types_are_send_and_sync() {
    // the type representation, which is `Arc`-shared throughout
    assert_send_sync::<Type>();

    // the AST, likewise
    assert_send_sync::<BlockNode>();

    // the interner and every symbol table hang off this
    assert_send_sync::<SemanticStateDatabase>();

    // the diagnostic collector, previously an `UnsafeCell` and the single
    // reason the whole pipeline was unshareable
    assert_send_sync::<JarvilProgramAnalysisErrors>();

    // the plain-data diagnostics handed to editors
    assert_send_sync::<JarvilDiagnostic>();

    assert_send::<JarvilCode>();
    assert_sync::<JarvilCode>();
}

#[test]
fn analysis_results_can_cross_a_thread_boundary() {
    let source = "def main():\n    let x: int = \"oops\"\n";

    // produced here, moved to another thread, read there
    let produced = diagnostics(source);

    let observed = thread::spawn(move || {
        produced
            .iter()
            .map(|diagnostic| diagnostic.message.clone())
            .collect::<Vec<_>>()
    })
    .join()
    .unwrap();

    assert_eq!(observed.len(), 1);
    assert!(observed[0].contains("does not match"));
}

#[test]
fn many_analyses_run_concurrently() {
    // Each source is distinct so that a shared-state bug shows up as crossed
    // results rather than as everything coincidentally agreeing.
    let sources: Vec<String> = (0..16)
        .map(|n| format!("def main():\n    let v{}: int = \"bad\"\n", n))
        .collect();

    let sources = Arc::new(sources);

    let handles: Vec<_> = (0..16)
        .map(|n| {
            let sources = Arc::clone(&sources);

            thread::spawn(move || {
                with_analysis(&sources[n], |ctx| {
                    (ctx.diagnostics.len(), ctx.source.to_string())
                })
            })
        })
        .collect();

    for (n, handle) in handles.into_iter().enumerate() {
        let (count, source) = handle.join().expect("analysis thread panicked");

        assert_eq!(count, 1, "thread {} saw the wrong diagnostic count", n);
        assert!(
            source.contains(&format!("v{}:", n)),
            "thread {} analysed the wrong source: {}",
            n,
            source
        );
    }
}

#[test]
fn a_shared_analysis_is_readable_from_several_threads_at_once() {
    // The interner is behind a lock and is read constantly during formatting,
    // so exercise several readers against one shared result.
    let source = "\
def add(x: int, y: int) -> int:
    return x + y

def main():
    let total = add(1, 2)
    let wrong: str = total
";

    let produced = Arc::new(diagnostics(source));

    let handles: Vec<_> = (0..8)
        .map(|_| {
            let produced = Arc::clone(&produced);

            thread::spawn(move || produced.len())
        })
        .collect();

    for handle in handles {
        assert_eq!(handle.join().unwrap(), produced.len());
    }

    assert_eq!(produced.len(), 1);
}
