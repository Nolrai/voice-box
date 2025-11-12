# 🧩 Migrating from **Voice** to **VoiceBox**

## Overview

The **Voice** and **VoiceBox** modules both deal with sound and language, but they serve different stages of the workflow.
This guide outlines how to restructure and merge your codebase into a unified, modern system — keeping the best of both.

---

## Current Layout

src-lib/
├── Voice
│ ├── IPA
│ │ ├── Common.hs
│ │ ├── PhonoCode.hs
│ │ ├── Roman.hs
│ │ └── Types.hs
│ ├── IPA.hs
│ ├── Synth.hs
│ ├── Types.hs
│ └── Util.hs
└── VoiceBox
├── Analyze.hs
├── Pipeline.hs
├── Synthesize.hs
├── Transform.hs
└── Types.hs


**Problem:**
- `Voice` handles **text → phoneme → synthesized sound** using a custom IPA and vocoder chain.
- `VoiceBox` handles **real audio → transformed audio** using analysis and resynthesis tools.
- Their names and structure overlap, causing conceptual confusion.

---

## Proposed Structure

### 1. Unify the namespaces
Rename modules to reflect a clear pipeline:

src-lib/
├── VoiceBox
│ ├── Audio
│ │ ├── Analyze.hs -- WAV → features (pitch, envelope, formants)
│ │ ├── Transform.hs -- feature → feature transforms
│ │ └── Synthesize.hs -- features → audio
│ ├── Language
│ │ ├── IPA
│ │ │ ├── Common.hs
│ │ │ ├── Roman.hs
│ │ │ ├── PhonoCode.hs
│ │ │ └── Types.hs
│ │ ├── TextToPhoneme.hs
│ │ └── Types.hs
│ ├── Pipeline.hs -- connects Language ↔ Audio
│ └── Types.hs


---

## Migration Steps

1. **Rename `Voice` → `VoiceBox.Language`**
   - Adjust imports (`Voice.IPA` → `VoiceBox.Language.IPA`).
   - Keep existing phonology and Romanization logic.

2. **Keep `VoiceBox` as the DSP layer**
   - This remains your core audio-to-audio and analysis pipeline.

3. **Introduce a top-level `VoiceBox.Pipeline`**
   - Connect text → phoneme → feature → audio.

4. **Clean up `Types.hs`**
   - Split shared data types:
     - `VoiceBox.Types.Audio` — features, envelopes, etc.
     - `VoiceBox.Types.Linguistic` — phonemes, symbols, language metadata.

5. **Re-export common interfaces**
   - From `VoiceBox.hs`:
     ```haskell
     module VoiceBox
       ( module VoiceBox.Language
       , module VoiceBox.Audio
       , synthesizeSpeech
       , analyzeAudio
       , transformFeatures
       ) where
     ```
     This lets you write simple scripts like:
     ```haskell
     import VoiceBox

     main = do
       features <- analyzeAudio "sample.wav"
       output   <- synthesizeSpeech features
       saveWav "output.wav" output
     ```

---

## Rationale

- **`VoiceBox`** becomes the umbrella project — it now mirrors the actual conceptual structure: language meets sound.
- **`Language`** and **`Audio`** layers can evolve independently but share a unified pipeline.
- This refactor keeps old `Voice` logic functional while clearly separating experimental and production paths.

---

## Next Steps

- Move your `Lexurgy`-related phoneme rules into `VoiceBox.Language`.
- Begin connecting `Analyze` → `Synthesize` with placeholder transforms.
- Once stable, retire the old `Voice` tree or freeze it under `VoiceLegacy`.

---

**Goal:**
A single ecosystem where:
- The **linguistic** front-end can describe what the Dolls *intend* to say.
- The **acoustic** back-end models how they *sound* when saying it.

That harmony between structure and sound will make future experiments—like ProtoDoll-alpha phonology—much easier to prototype.

---
