// Executes the transpiled Python in the browser, via Pyodide.
//
// Worth being explicit about why this is a second WebAssembly module: compiling
// Jarvil to wasm gets you Python *source*, not the ability to run it. Running it
// needs a Python interpreter, and in a browser that means Pyodide -- CPython
// itself compiled to wasm.
//
// It is roughly ten megabytes against the Jarvil compiler's ~850 KB, so it is
// loaded lazily on the first Run and never during ordinary browsing of the
// docs.

const PYODIDE_VERSION = "0.27.2";
const PYODIDE_URL = `https://cdn.jsdelivr.net/pyodide/v${PYODIDE_VERSION}/full/`;

let pyodide = null;
let loading = null;

/**
 * Loads Pyodide, once.
 *
 * `onProgress` is called with a human-readable status, since this download is
 * slow enough that silence looks like a hang.
 */
export async function loadPython(onProgress = () => {}) {
  if (pyodide) return pyodide;

  if (!loading) {
    loading = (async () => {
      onProgress("Downloading Python runtime (~10 MB, first run only)…");

      // Loaded from a CDN script tag rather than bundled: Pyodide ships its own
      // wasm and data files and expects to fetch them relative to `indexURL`.
      if (!globalThis.loadPyodide) {
        await new Promise((resolve, reject) => {
          const script = document.createElement("script");
          script.src = `${PYODIDE_URL}pyodide.js`;
          script.onload = resolve;
          script.onerror = () => reject(new Error("failed to load Pyodide"));
          document.head.appendChild(script);
        });
      }

      onProgress("Starting Python…");
      pyodide = await globalThis.loadPyodide({ indexURL: PYODIDE_URL });

      return pyodide;
    })();
  }

  return loading;
}

/**
 * Runs `source` and returns everything it printed.
 *
 * Both streams are captured rather than only stdout, so a traceback from the
 * generated code is shown instead of vanishing into the console. A Python-level
 * exception is reported as output rather than thrown: a program that raises has
 * still run, and the traceback is the result the user wants to see.
 */
export async function runPython(source, onProgress = () => {}) {
  const py = await loadPython(onProgress);

  onProgress("Running…");

  let stdout = "";
  let stderr = "";

  py.setStdout({ batched: (text) => (stdout += text + "\n") });
  py.setStderr({ batched: (text) => (stderr += text + "\n") });

  try {
    await py.runPythonAsync(source);
  } catch (error) {
    // Pyodide surfaces a Python traceback as the error message.
    stderr += String(error.message ?? error);
  } finally {
    py.setStdout({});
    py.setStderr({});
  }

  return { stdout, stderr };
}

/** Whether the runtime is already in memory, so the UI can label the button. */
export function isPythonLoaded() {
  return pyodide !== null;
}
