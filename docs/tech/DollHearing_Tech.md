## 6. Standard Resonator Types and Frequency Bands

The Doll's auditory system is composed of three main types of resonators, each covering a distinct frequency range and physical/magical mechanism. This structure is inspired by both biological cochleae and engineered filterbanks, but adapted to the materials and phase-based perception of Dolls.

### Resonator Types

| Type              | Material/Structure         | Frequency Range (Hz) | Notes |
|-------------------|---------------------------|----------------------|-------|
| Drum              | Red ink pool/reservoir    | 360–1800             | Large, low-frequency, main auditory resonator |
| BismuthWrapping   | Bismuth wire, alchemical  | 2000–11200           | Mid-frequency, self-catalyzing pulse transmission |
| ShellResonance    | Ceramic/porcelain shell   | 6000–20000           | High-frequency, physical shell vibration |

Each resonator is modeled as a phase-saturated cavity, with phase (not amplitude) as the primary variable. The Haskell model uses:

```haskell
data CavityType = Drum | BismuthWrapping | ShellResonance deriving (Eq, Show)
data ResonantCavity = ResonantCavity { cavityType :: CavityType, cavityFrequency :: Double, cavityPhase :: Double }
```

### Standard Band Distribution (Example)

The following set of bands provides broad, speech-optimized coverage:

| Type            | Frequencies (Hz)                |
|-----------------|---------------------------------|
| Drum            | 360, 500, 700, 1000, 1400, 1800 |
| BismuthWrapping | 2000, 2800, 4000, 5600, 8000, 11200 |
| ShellResonance  | 6000, 9000, 13000, 18000, 20000 |

This distribution is logarithmic, with more area (or more sensors) devoted to lower frequencies, and more bands per octave at higher frequencies, matching both biological and engineering best practices.

#### Physical and Magical Rationale
- Drums (red ink pools) act as the main low-frequency sensors, analogous to the basilar membrane in mammals.
- BismuthWrapping covers the midrange, using alchemical pulses at speeds similar to myelinated nerves, filling the gap between Drums and ShellResonance.
- ShellResonance provides high-frequency sensitivity, with resonances determined by the shell's size and material.

This model allows Dolls to perceive a wide range of frequencies, with phase coherence as the primary perceptual variable, and is implemented in code as the `standardHearingCavities` list in `VoiceBox.Audio.Ear`.
# Doll Hearing — Technical Basis

This document covers the physical and perceptual mechanisms of Doll hearing, focusing on phase-based detection, material structure, and consequences for sound processing.

## 1. Two Modes of Hearing: Power vs Phase
- Humans sense amplitude ("how strong is the vibration?") via hair cells and pressure transduction.
- Dolls sense phase/coherence ("how shifted or delayed are returning waves?") via ink–metal pathways and interferometric lattices.

| Property | Human (amplitude) | Doll (phase) |
|---|---:|---|
| Medium | Air pressure waves | Electromagnetic/vibrational fields, body lattice |
| Sensor | Hair cells (displacement magnitude) | Ink–metal traces (phase shift between oscillators) |
| Primary data | Loudness, envelope, rhythm | Phase drift, coherence, detuning |
| Perceptual space | Time domain | Frequency / phase domain |

## 2. Material and Mechanical Basis

The lattice is not a microphone but an interferometer. Common materials and their roles:

| Material | Function | Physical/Story Notes |
|---|---|---|
| Porcelain | Structural resonator | High-Q resonances; can be piezoelectric if doped; provides fixation and memory scaffolding |
| Bismuth wires | Signal pathways | Anisotropic, birefringent; acts as mirror-like conductors that preserve phase relationships |
| Alchemical ink | Pattern tuner | Semi-conductive pigment that marks and configures flow of fields and intent |
| Oil | Delay medium | Dielectric that slows collapse and sustains oscillation ("dream fluid") |

Ink channels and metal inlays form conductive, reactive pathways that store and release phase. The body acts as a distributed interferometer with many comparison nodes; perception arises across a torso-wide network rather than a single ear.

## 3. Why Phase Is Cheap, Power Is Dear

Because the lattice naturally resonates, maintaining coherence costs little. Observing or amplifying amplitude — collapsing the pattern into a definite power measurement — consumes attention and internal energy. Practically:
- Sustaining a stable tone/pattern is restful for a Doll.
- Attempting to reproduce amplitude-rich human speech is exhausting and short-lived.

This energy economy shapes behavior, language design, and social dynamics.

## 4. Perceptual Consequences

1. Narrow effective frequency band: Typically bound to the carrier's lower harmonics (e.g. ~60–1200 Hz). High-frequency consonants and sharp transients are attenuated or lost.
2. Speech comprehension: Dolls track phase relationships of vowels and harmonics rather than consonant edges; human speech may be perceived as beating textures.
3. Voice production: Speaking is phase-matching internal resonances to external expectations; this produces metallic, harmonic-rich voices in human ears.
4. Emotional mapping: Phase coherence correlates with comfort and trust; discordance maps to pain, confusion, or alarm.

## 5. Comparison Table (Condensed)

| Property | Humans | Dolls |
|---|---:|---|
| Mechanism | Pressure transduction | Interference / phase comparison |
| Primary data | Amplitude | Phase drift, coherence |
| Easy to sense | Loudness / timbre | Micro-timing, direction, coherence |
| Hard to sense | Phase | Power / loudness |
## References & Cross-links

- Consolidates material from `DollHearing_PhaseOverPower.md` and `TheNatureOfTheirHearing.md`.
- Cross-reference: [PreDoll0_Speech.md](../lore/PreDoll0_Speech.md), [VoiceEvolution_TechnicalAndPsychic.md](VoiceEvolution_TechnicalAndPsychic.md), [../concept/DollHearing_Concepts.md](../concept/DollHearing_Concepts.md)

## See Also
- [../lore/DollHearing_Lore.md](../lore/DollHearing_Lore.md)
- [../concept/DollHearing_Concepts.md](../concept/DollHearing_Concepts.md)
- [VoiceEvolution_TechnicalAndPsychic.md](VoiceEvolution_TechnicalAndPsychic.md)
