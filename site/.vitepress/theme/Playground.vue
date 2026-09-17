<script setup>
import { onBeforeUnmount, onMounted, ref, shallowRef } from "vue";

const props = defineProps({
  initial: { type: String, default: "" },
  height: { type: String, default: "420px" },
});

const editorHost = ref(null);
const view = shallowRef(null);

const output = ref("");
const outputKind = ref("idle"); // idle | running | ok | error
const status = ref("");
const python = ref("");
const showPython = ref(false);
const ready = ref(false);

let bridge = null;
let runtime = null;

const SAMPLE = `// Jarvil catches this before the program ever runs.
// Try changing 42 to "world".

def greet(name: str) -> str:
    return "Hello, " + name

def main():
    print(greet("world"))
    print(greet(42))
`;

onMounted(async () => {
  // Everything here is browser-only: VitePress renders these pages on the
  // server at build time, where CodeMirror and WebAssembly are unavailable.
  const [
    {
      EditorView,
      keymap,
      lineNumbers,
      highlightActiveLine,
      hoverTooltip,
      drawSelection,
      dropCursor,
      rectangularSelection,
      crosshairCursor,
    },
    { EditorState },
    { defaultKeymap, history, historyKeymap, indentWithTab },
    { linter, lintGutter },
    { syntaxHighlighting, HighlightStyle, indentUnit },
    { tags },
    { jarvil },
    bridgeModule,
    runtimeModule,
  ] = await Promise.all([
    import("@codemirror/view"),
    import("@codemirror/state"),
    import("@codemirror/commands"),
    import("@codemirror/lint"),
    import("@codemirror/language"),
    import("@lezer/highlight"),
    import("../../src/jarvil-language.js"),
    import("../../src/jarvil-wasm-bridge.js"),
    import("../../src/python-runtime.js"),
  ]);

  bridge = bridgeModule;
  runtime = runtimeModule;

  // Diagnostics, recomputed as you type. The compiler reports every error
  // rather than stopping at the first, so all of them get a squiggle.
  const jarvilLinter = linter(async (v) => {
    const source = v.state.doc.toString();

    try {
      const found = await bridge.diagnostics(source);

      return found.map((d) => ({
        from: d.from,
        to: Math.max(d.to, d.from + 1),
        severity: d.severity === "advice" ? "info" : d.severity,
        message: d.help ? `${d.message}\n\n${d.help}` : d.message,
      }));
    } catch (error) {
      return [];
    }
  }, { delay: 250 });

  // Hover: the same signature-and-doc-comment text the editors show.
  const jarvilHover = hoverTooltip(async (v, pos) => {
    const source = v.state.doc.toString();

    try {
      const found = await bridge.hover(source, pos);
      if (!found) return null;

      return {
        pos: found.from,
        end: found.to,
        above: true,
        create() {
          const dom = document.createElement("div");
          dom.className = "jv-hover";
          // contents is markdown with a fenced code block; strip the fence and
          // render as preformatted text rather than pulling in a md parser
          dom.textContent = found.contents
            .replace(/```jarvil\n?/g, "")
            .replace(/```/g, "")
            .trim();
          return { dom };
        },
      };
    } catch (error) {
      return null;
    }
  });

  // Go to definition, on the usual modifier-click.
  const gotoDefinition = EditorView.domEventHandlers({
    mousedown(event, v) {
      if (!(event.metaKey || event.ctrlKey)) return false;

      const pos = v.posAtCoords({ x: event.clientX, y: event.clientY });
      if (pos === null) return false;

      const source = v.state.doc.toString();

      bridge.definition(source, pos).then((found) => {
        if (!found) return;

        v.dispatch({
          selection: { anchor: found.targetFrom, head: found.targetTo },
          scrollIntoView: true,
        });
      });

      event.preventDefault();
      return true;
    },
  });

  view.value = new EditorView({
    state: EditorState.create({
      doc: props.initial || SAMPLE,
      extensions: [
        lineNumbers(),
        highlightActiveLine(),
        // Without these the caret is the browser's, which cannot be themed and
        // disappears against some backgrounds.
        drawSelection(),
        dropCursor(),
        rectangularSelection(),
        crosshairCursor(),
        history(),
        lintGutter(),
        indentUnit.of("    "),
        keymap.of([...defaultKeymap, ...historyKeymap, indentWithTab]),
        syntaxHighlighting(
          HighlightStyle.define([
            { tag: tags.comment, color: "var(--jv-comment)", fontStyle: "italic" },
            { tag: tags.keyword, color: "var(--jv-keyword)" },
            { tag: tags.definitionKeyword, color: "var(--jv-keyword)", fontWeight: "600" },
            { tag: tags.typeName, color: "var(--jv-type)" },
            { tag: tags.string, color: "var(--jv-string)" },
            { tag: tags.number, color: "var(--jv-number)" },
            { tag: tags.bool, color: "var(--jv-number)" },
            { tag: tags.self, color: "var(--jv-keyword)", fontStyle: "italic" },
            { tag: tags.function(tags.variableName), color: "var(--jv-function)" },
            { tag: tags.operator, color: "var(--jv-operator)" },
            { tag: tags.punctuation, color: "var(--jv-punctuation)" },
            { tag: tags.invalid, color: "var(--vp-c-danger-1)", textDecoration: "underline wavy" },
          ]),
          { fallback: true },
        ),
        jarvil,
        jarvilLinter,
        jarvilHover,
        gotoDefinition,
        EditorView.theme({
          "&": {
            height: "100%",
            fontSize: "14px",
            backgroundColor: "var(--vp-c-bg)",
            color: "var(--vp-c-text-1)",
          },
          ".cm-scroller": {
            fontFamily: "var(--vp-font-family-mono)",
            lineHeight: "1.6",
          },
          ".cm-gutters": {
            backgroundColor: "var(--vp-c-bg-soft)",
            color: "var(--vp-c-text-3)",
            border: "none",
            borderRight: "1px solid var(--vp-c-divider)",
          },
          ".cm-activeLineGutter": {
            backgroundColor: "var(--vp-c-bg-elv)",
            color: "var(--vp-c-text-1)",
          },
          ".cm-activeLine": { backgroundColor: "var(--jv-active-line)" },
          ".cm-content": { caretColor: "var(--vp-c-brand-1)" },
          ".cm-cursor, .cm-dropCursor": {
            borderLeftColor: "var(--vp-c-brand-1)",
            borderLeftWidth: "2px",
          },
          "&.cm-focused .cm-cursor": { borderLeftColor: "var(--vp-c-brand-1)" },
          ".cm-selectionBackground": { backgroundColor: "var(--jv-selection)" },
          "&.cm-focused > .cm-scroller > .cm-selectionLayer .cm-selectionBackground":
            { backgroundColor: "var(--jv-selection)" },
          ".cm-content ::selection, .cm-line ::selection": {
            backgroundColor: "var(--jv-selection)",
          },
          ".cm-tooltip": {
            backgroundColor: "var(--vp-c-bg-elv)",
            border: "1px solid var(--vp-c-divider)",
            borderRadius: "6px",
            color: "var(--vp-c-text-1)",
          },
          ".cm-tooltip .cm-tooltip-arrow:before": {
            borderTopColor: "var(--vp-c-divider)",
          },
          ".cm-tooltip .cm-tooltip-arrow:after": {
            borderTopColor: "var(--vp-c-bg-elv)",
          },
        }),
      ],
    }),
    parent: editorHost.value,
  });

  // Warm the compiler so the first keystroke is not the thing that downloads it.
  await bridge.loadCompiler();
  ready.value = true;
});

onBeforeUnmount(() => view.value?.destroy());

async function run() {
  if (!bridge || !runtime) return;

  outputKind.value = "running";
  output.value = "";
  status.value = "Compiling…";

  const source = view.value.state.doc.toString();
  const result = await bridge.compile(source);

  python.value = result.python ?? "";

  if (!result.python) {
    outputKind.value = "error";
    status.value = "";
    output.value = result.diagnostics
      .map((d) => (d.help ? `${d.message}\n  help: ${d.help}` : d.message))
      .join("\n\n");
    return;
  }

  try {
    const { stdout, stderr } = await runtime.runPython(result.python, (message) => {
      status.value = message;
    });

    status.value = "";
    outputKind.value = stderr ? "error" : "ok";
    output.value = (stdout + stderr).trimEnd() || "(no output)";
  } catch (error) {
    status.value = "";
    outputKind.value = "error";
    output.value = String(error);
  }
}

function reset() {
  view.value?.dispatch({
    changes: { from: 0, to: view.value.state.doc.length, insert: props.initial || SAMPLE },
  });
  output.value = "";
  python.value = "";
  outputKind.value = "idle";
}
</script>

<template>
  <div class="jv-playground" :style="{ height }">
    <div class="jv-toolbar">
      <button class="jv-run" :disabled="!ready || outputKind === 'running'" @click="run">
        {{ outputKind === "running" ? "Running…" : "▶ Run" }}
      </button>
      <button class="jv-secondary" @click="reset">Reset</button>
      <button
        class="jv-secondary"
        :disabled="!python"
        @click="showPython = !showPython"
      >
        {{ showPython ? "Hide" : "Show" }} Python
      </button>
      <span class="jv-hint">⌘/Ctrl-click an identifier to jump to its definition</span>
    </div>

    <div ref="editorHost" class="jv-editor" />

    <div v-if="status" class="jv-status">{{ status }}</div>

    <pre v-if="showPython && python" class="jv-python">{{ python }}</pre>

    <pre v-if="output" class="jv-output" :class="outputKind">{{ output }}</pre>
  </div>
</template>

<style>
/* The docs column is sized for prose. On the playground page it is widened so
   the editor has room, while the surrounding text stays readable. */
.playground-page .VPDoc .container,
.playground-page .VPDoc .content,
.playground-page .VPDoc .content-container {
  max-width: 100% !important;
}

.playground-page .VPDoc {
  padding-left: 24px;
  padding-right: 24px;
}

@media (min-width: 960px) {
  .playground-page .VPDoc {
    padding-left: 48px;
    padding-right: 48px;
  }
}

/* Keep the prose itself from stretching to unreadable line lengths; only the
   playground uses the full width. */
.playground-page .vp-doc > div > :not(.jv-playground):not(.custom-block):not(h1):not(h2) {
  max-width: 760px;
}

.playground-page .vp-doc > div > h1,
.playground-page .vp-doc > div > h2 {
  max-width: 760px;
}

/* Syntax colours, defined for both themes so the editor follows the site's
   light/dark toggle rather than assuming a light page. */
:root {
  --jv-comment: #6a737d;
  --jv-keyword: #d73a49;
  --jv-type: #6f42c1;
  --jv-string: #032f62;
  --jv-number: #005cc5;
  --jv-function: #6f42c1;
  --jv-operator: #d73a49;
  --jv-punctuation: #24292e;
  --jv-selection: #b6d7ff;
  /* translucent: this paints over the selection layer */
  --jv-active-line: rgba(0, 0, 0, 0.04);
}

.dark {
  --jv-comment: #8b949e;
  --jv-keyword: #ff7b72;
  --jv-type: #ffa657;
  --jv-string: #a5d6ff;
  --jv-number: #79c0ff;
  --jv-function: #d2a8ff;
  --jv-operator: #ff7b72;
  --jv-punctuation: #c9d1d9;
  --jv-selection: #2d4f76;
  /* translucent: this paints over the selection layer */
  --jv-active-line: rgba(255, 255, 255, 0.045);
}

.jv-playground {
  display: flex;
  flex-direction: column;
  min-height: 320px;
  border: 1px solid var(--vp-c-divider);
  border-radius: 8px;
  overflow: hidden;
  margin: 16px 0;
}

.jv-editor {
  flex: 1;
  min-height: 0;
  overflow: hidden;
}

.jv-toolbar {
  display: flex;
  gap: 8px;
  align-items: center;
  padding: 8px;
  background: var(--vp-c-bg-soft);
  border-bottom: 1px solid var(--vp-c-divider);
  flex-wrap: wrap;
}

.jv-toolbar button {
  border-radius: 6px;
  padding: 4px 12px;
  font-size: 13px;
  font-weight: 500;
  border: 1px solid var(--vp-c-divider);
  cursor: pointer;
}

.jv-toolbar button:disabled {
  opacity: 0.5;
  cursor: default;
}

.jv-run {
  background: var(--vp-c-brand-1);
  color: white;
  border-color: var(--vp-c-brand-1) !important;
}

.jv-secondary {
  background: var(--vp-c-bg);
  color: var(--vp-c-text-1);
}

.jv-hint {
  margin-left: auto;
  font-size: 12px;
  color: var(--vp-c-text-3);
}

.jv-editor .cm-editor {
  background: var(--vp-c-bg);
}

.jv-editor .cm-editor.cm-focused {
  outline: none;
}

.jv-status,
.jv-output,
.jv-python {
  margin: 0;
  padding: 10px 12px;
  font-family: var(--vp-font-family-mono);
  font-size: 13px;
  white-space: pre-wrap;
  border-top: 1px solid var(--vp-c-divider);
}

.jv-status {
  color: var(--vp-c-text-2);
  background: var(--vp-c-bg-soft);
}

.jv-python {
  background: var(--vp-c-bg-alt);
  color: var(--vp-c-text-2);
  max-height: 240px;
  overflow: auto;
}

.jv-output.ok {
  background: var(--vp-c-bg-soft);
}

.jv-output.error {
  background: var(--vp-c-danger-soft);
  color: var(--vp-c-danger-1);
}

.jv-hover {
  padding: 6px 10px;
  font-family: var(--vp-font-family-mono);
  font-size: 13px;
  white-space: pre-wrap;
  max-width: 480px;
}
</style>
