//! Free functions available to every Jarvil program without an import.
//!
//! Each one is a thin front for a Python builtin of the same name. That is not a
//! coincidence and it is worth stating plainly: code generation emits these
//! verbatim, because builtins are registered with no unique id and so skip name
//! mangling. A builtin declared here as `len` becomes a literal `len(..)` in the
//! generated Python. The consequence is that a builtin may only be added when a
//! Python function of the same name has the same arity, semantics and return
//! type -- otherwise the program type-checks and then fails at runtime, which is
//! the opposite of the point of the language.

use crate::{
    constants::common::{FLOAT, INT, STRING},
    core::string_interner::Interner,
    scope::symbol::{
        function::{CallableData, CallableKind},
        interfaces::InterfaceBounds,
        types::generic_ty::GenericTypeParams,
    },
    types::{core::Type, helper::unbounded_generic_ty_in_func_with_decl_index},
};
use rustc_hash::FxHashMap;
use text_size::TextRange;

// A non-generic free function.
fn func(params: Vec<Type>, return_ty: Type) -> CallableData {
    CallableData::new(params, return_ty, CallableKind::Function, None)
}

// A free function generic over one unbounded type parameter.
//
// Unbounded means unchecked: `len` will accept an `int` and then fail in
// Python. Jarvil has no `Sized`-style interface to bound this with yet, and the
// alternative -- no `len` at all -- is worse. Once interface bounds can express
// "has a length", these should be tightened.
fn generic_func(interner: &Interner, params: Vec<Type>, return_ty: Type) -> CallableData {
    CallableData::new(
        params,
        return_ty,
        CallableKind::Function,
        Some(GenericTypeParams::new(vec![(
            interner.intern("V"),
            InterfaceBounds::default(),
            TextRange::default(),
        )])),
    )
}

/// Every free function available without an import.
///
/// Registered into the global scope when a [`SemanticStateDatabase`] is
/// created. See the module documentation for why these names must match
/// Python's exactly.
///
/// [`SemanticStateDatabase`]: crate::scope::semantic_db::SemanticStateDatabase
pub fn builtin_funcs(interner: &Interner) -> FxHashMap<&'static str, CallableData> {
    let mut funcs = FxHashMap::default();

    let generic = || unbounded_generic_ty_in_func_with_decl_index(0, interner);
    let int = || Type::new_with_atomic(INT);
    let float = || Type::new_with_atomic(FLOAT);
    let string = || Type::new_with_atomic(STRING);

    // print<V>(obj: V)
    funcs.insert(
        "print",
        generic_func(interner, vec![generic()], Type::new_with_void()),
    );

    // len<V>(obj: V) -> int
    //
    // Deliberately a free function rather than a `.len()` method: Python spells
    // it `len(x)`, and matching that keeps the emitted code a direct
    // translation rather than something codegen has to rewrite.
    funcs.insert("len", generic_func(interner, vec![generic()], int()));

    // range(start: int, end: int) -> [int]
    funcs.insert(
        "range",
        func(vec![int(), int()], Type::new_with_array(int())),
    );

    // abs(x: int) -> int
    funcs.insert("abs", func(vec![int()], int()));

    // min(a: int, b: int) -> int
    funcs.insert("min", func(vec![int(), int()], int()));

    // max(a: int, b: int) -> int
    funcs.insert("max", func(vec![int(), int()], int()));

    // sum(l: [int]) -> int
    funcs.insert("sum", func(vec![Type::new_with_array(int())], int()));

    // input() -> str
    funcs.insert("input", func(vec![], string()));

    // ord(c: str) -> int
    funcs.insert("ord", func(vec![string()], int()));

    // chr(code: int) -> str
    funcs.insert("chr", func(vec![int()], string()));

    // round(x: float) -> int
    funcs.insert("round", func(vec![float()], int()));

    funcs
}

// Not registered, because they cannot be called:
//
// `bool`, `int`, `float`, `str` are atomic *type* keywords, so the lexer emits
// `<atomic-type>` and `bool(x)` is a syntax error before it ever reaches name
// resolution. Registering them would put entries in the namespace that no
// program can reach.
//
// This is why Jarvil has no conversion functions. Supporting them means either
// teaching the parser that an atomic type in expression position is a call, or
// giving conversions different names (`to_int`) at the cost of no longer
// matching Python -- which is what lets code generation emit these verbatim.
