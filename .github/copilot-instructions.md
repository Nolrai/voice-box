## Quick context

This repository is a small Haskell synthesis project named `voice-box`. It contains
1) a library (`src-lib`) with the core parsing and synthesis logic under `Voice/`,
2) stub modules for audio analysis under `VoiceBox/` (future feature extraction from WAV files),
3) an executable (`src-exe/Main.hs`) that ties the library to CLI behavior, and
4) a small test suite (`src-test`) plus example test inputs in `test-data/`.

Key modules to read first:
- `src-lib/Voice/IPA.hs` — top-level parsing glue
- `src-lib/Voice/IPA/` — Megaparsec parsers (Roman, PhonoCode, Common, Types)
- `src-lib/Voice/Synth.hs` and `src-lib/Voice/Types.hs` — how parsed data maps to audio
- `src-lib/Voice/Util.hs` — I/O helpers used across the project
- `src-lib/VoiceBox/` — stub modules for future audio analysis/resynthesis (Analyze, Synthesize, Pipeline, Types)
- `src-exe/Main.hs` — program entrypoint and CLI wiring

Why things are structured this way
- Parsing and synthesis are split: parsing lives under `Voice.IPA.*` and produces typed
  intermediate structures (`IPA.Types`, `Voice.Types`). `Synth` consumes those types and drives
  the audio backend (`lambdasound`). This separation keeps parsing, representation and synthesis
  logic independent and testable.
- VoiceBox modules are currently stubs for future audio analysis/feature extraction work (analyzing
  real audio files and extracting pitch, formants, envelopes, etc. for hybrid synthesis).

Build / run / test (practical commands)

- Build the project: `cabal v2-build` (or `cabal build`) — uses `voice-box.cabal`.
- Run the executable: `cabal v2-run voice-box -- <args>` (or `cabal run voice-box -- <args>`).
- Open a REPL for interactive development: `cabal v2-repl` or `cabal repl :l src-exe/Main.hs`.
- Run tests: `cabal v2-test` (test suite defined in `test-suite` stanza uses `src-test/Main.hs`).

Project-specific conventions & warnings

- Source layout: library sources live in `src-lib`, the executable in `src-exe`, and tests in `src-test`.
- When you add a module you must update `voice-box.cabal`:
  - add exported modules to `exposed-modules` if they belong to the public API,
  - otherwise add them to `other-modules` so Cabal will compile them.
- The project compiles against `GHC2024` in the cabal file; the local build cache is in `dist-newstyle/`.
- `-Wall` is enabled via the `common warnings` stanza in the cabal file. Keep code warning-free where possible.

Parsing & testing notes (examples from repo)

- Parsers use Megaparsec — look at `src-lib/Voice/IPA/*.hs` for patterns (combinators, error handling).
- There are golden/test inputs under `test-data/golden/parse_golden.txt` and sample inputs in `test-data/misc/`.
  Use these files when writing parser regressions.

Integration points & external dependencies

- Audio backend: `lambdasound` (declared in `build-depends`). Changes to synthesis or IO will likely require
  running the program to verify sound output. If tests simulate audio, they will usually exercise only
  the synthesis data types rather than full audio playback.
- No external network services appear to be used; primary external deps are Haskell packages (Megaparsec, text, bytestring).

Small coding contract for contributors/agents

- Inputs: UTF-8 romanized text sample files under `test-data/misc` or the CLI input parsed by `Voice.IPA`.
- Outputs: typed parse trees (`Voice.IPA.Types`) and synthesis-ready structures (`Voice.Types`), and audio via `lambdasound`.
- Error modes: parsers should return informative Megaparsec errors; synthesis should validate required fields.

Useful quick references

- Main entrypoints: `src-exe/Main.hs`, `src-lib/Voice/IPA.hs`, `src-lib/Voice/Synth.hs`.
- Cabal file: `voice-box.cabal` — update modules/build-depends/language there.
- Tests & examples: `src-test/`, `test-data/golden/`, `test-data/misc/`.
- Formatting: there is an `ormolu` file in the repo root (use if present to format Haskell sources).

If something seems missing

- If you need additional developer scripts or CI instructions (formatting, pre-commit, reproducible build matrix), say which commands you run locally and I will add them to this file.

Next step for me: I'll update or extend this doc if you want more examples (small code snippets showing parse → synth flow), or add CI-friendly commands.
