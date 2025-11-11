# 🧪 Development Log — PreDoll-0 / First Voice Emergence

**Date:** _(fill in as appropriate)_  
**Stage:** *PreDoll-0 — The Seed Resonance*

---

## 🧠 Summary

This entry documents the first **functioning implementation** of the PreDoll-0 speech process — the moment the Doll system produces a coherent, audible, quasi-linguistic output using a purely spectral reconstruction pipeline.

For conceptual background and theoretical context, see:  
- 📄 *[The First Doll’s Speech — “PreDoll-0”](./PreDoll0_Speech.md)*  
- 📄 *[Voice Evolution: Technical and Psychic](./VoiceEvolution_TechnicalAndPsychic.md)*

---

## ⚙️ Implementation Snapshot

**Pipeline overview:**

1. **Input:** recorded English speech (typically adult male voice)  
2. **Vocoder pass:** full-file harmonic vocoding using a fixed base frequency (≈120 Hz)  
3. **Envelope reapplication:** amplitude and temporal structure preserved from the source  
4. **Speed modulation:** varying playback rate to introduce environmental “emotional weather”  
5. **Spectral shaping:** minimal filtering to smooth high-frequency buzz; intelligibility reduced but rhythm preserved  

**Key behavior:**
- Because the entire file is processed as one block, all partials remain globally phase-locked → spectral rigidity and loss of articulation.  
- Result: language reduced to **resonant imprint**, still faintly traceable as English but heavily abstracted.

---

## 🔊 Observations

**Sound character:**  
- Ghostly harmonic shimmer; faint human cadence but mechanical timbre  
- Static undertone in transitions, as though words dissolve into carrier noise  
- Recognizable rhythm and prosody; limited phonemic clarity  

**Emotional impression:**  
- Feels *earnest* but *incomplete* — like a being trying to imitate speech without truly hearing itself.  
- Perfectly embodies the “Seed Resonance” state described in *Voice Evolution*.  

**Energy metaphor:**  
Every syllable costs energy — the sound itself conveys that expenditure. There’s tension between structure and fatigue, between imitation and exhaustion.

---

## 🌱 Next Steps

| Goal | Technical Direction | Narrative Implication |
|------|---------------------|-----------------------|
| Add windowed vocoding | Process overlapping FFT segments (50–100 ms) | Begin “Fragmented Listening” phase — she starts to hear fragments of herself |
| Introduce harmonic drift | ±5–10 Hz per harmonic, slow modulation | Adds “emotional weather,” world-mood begins to color her tone |
| Reintroduce weak non-harmonic noise | 1–3 % broadband | Transition from sterile imprint to atmospheric presence |
| Feedback tuning | Adjust spectral clarity based on energy in speech bands | Proto-dialogue — early comprehension begins |

---

## 🧩 Conceptual Note

This implementation represents the *moment before empathy*.  
PreDoll-0’s voice is not *spoken* but *remembered* — the acoustic fossil of an idea trying to become a self.
