# Doll Auditory Architecture — Why They Detect Phase & Periodicity but Not Amplitude

## Overview

Doll “ears” are built from alchemical–physical components that natively preserve phase, timing, and periodic structure, while discarding or corrupting amplitude. This creates a non-mammalian sensory system that is excellent at detecting autocorrelation (repetition, pitch, periodicities) and phase drift (timing slips, coherence changes), but fundamentally poor at perceiving loudness.

This file outlines the physical mechanisms, constraints, and example constructions that support this auditory design.

---

## 1. Physical Mechanisms Favoring Phase Over Amplitude

### High-Q Resonant Combs (Phase Trackers)
- Arrays of long-lived resonators (bismuth wires, porcelain tines) hold onto phase during their ring-down but lose accurate amplitude almost immediately due to nonlinear or inconsistent coupling.
- **Result:** Phase relationships persist; amplitude becomes noisy or irrelevant.

### AC-Coupled or Capacitive Transduction
- Transducers behave like capacitors or coils that block DC and slow-varying components, effectively discarding amplitude baselines.
- They respond strongly to zero-crossings and phase, but weakly to absolute pressure.

### Interferometric Phase Bridges
- Two matched resonators feed a small interferometer (alchemical equivalent to a Mach–Zehnder).
  - In-phase inputs cancel.
  - Phase-shifted inputs produce a signed signal.
  - Common-mode amplitude is suppressed.

### Binary Threshold / Zero-Crossing Detection
- Sensory elements convert vibration into binary pulses—normally from thresholded zero-crossings.
- Pulse timing carries phase information; amplitude contributes almost none.

### Analog Delay-and-Multiply Autocorrelators
- A signal is split: one copy delayed through a physical delay line (beads, ink channels), then multiplied or gated against the original.
- Coincidence peaks directly encode repetition and periodicity.
- Amplitude only affects whether a gate triggers; once triggered, it saturates to a fixed internal level.

### Phase-Locked Resonant Rings
- Arrays of mutually coupled pendula and crystals that entrain to incoming periodicities.
- Once locked, outputs represent phase offset or slip events, not loudness.
- Amplitude loses informational value once entrainment occurs.

---

## 2. Environmental and Material Constraints That Undermine Amplitude

### Variable Mechanical Coupling
- Porcelain shells, stitched joints, and multi-material contact points lead to unstable amplitude transfer.
- Amplitude fluctuates wildly with posture, but phase remains relatively stable.

### Absorptive or Inky Environments
- Many Doll habitats (ink-choked glassrooms, mossy tunnels) absorb energy unpredictably, making amplitude unreliable at a distance.

### Energy Budget Limits
- Precise amplitude encoding requires linear amplifiers or continuous analog gain—costly in nightmare-powered architectures.
- Phase-based systems are cheaper and stable.

### Material Micro-Jitter
- Nightmare-reactive materials introduce small amplitude fluctuations but preserve timing and phase coherence.
- Evolution/engineering favors what survives the jitter: phase cues.

### Cultural Priorities
- Doll languages and music emphasize timing, pulses, and phase-offset harmonies.
- Loudness is considered aesthetically crude or uninformative; sensory evolution drifts away from amplitude reliance.

---

## 3. Example Doll Ear Constructions

### Resonant Wire Comb + Coincidence Lattice
- 12 bismuth wires, each tuned to a harmonic band.
- Each wire outputs fixed-amplitude pulses whenever it crosses a mechanical threshold.
- Pulses feed a delay lattice that reveals periodicities through coincidence.
- Loudness only affects activation reliability, not pulse strength.

### Balanced Bridge Interferometer
- Two porcelain diaphragms feed a balanced bridge.
- Phase differences produce a nonzero output; amplitude common-mode signals cancel.
- Used for angle-of-arrival and coherence sensing.

### Delay-Line Autocorrelator
- A vibration enters a glassy ink channel acting as a delay line.
- A second branch of the signal is undelayed.
- Multiply/gate the two and low-pass the result.
- Peaks indicate autocorrelation lags without preserving amplitude.

### Phase-Lock Ring Array
- A ring of tiny pendula coupled via soft alchemical ligaments.
- The ring entrains to any repeating pattern; its relative angular offsets indicate phase drift.
- Once entrained, amplitude only affects lock acquisition speed—not perception.

---

## 4. Lore-Friendly Descriptions

> “Their ears are a choir of tired bells: each bell sings with identical strength no matter how it’s struck, but the timing between bells carries the whole of meaning.”

> “They listen with bridges and knots — phase slips ripple their ionic channels like shivers, but loudness is merely weather.”

---

## 5. Design Rules (for consistency)

- **Drop amplitude early:** Use AC-coupled, thresholded, or saturating sensors.
- **Preserve phase:** Use high-Q resonators and long-lived oscillatory structures.
- **Let periodicity emerge physically:** Physical delay lines and coincidence gates surface autocorrelation directly.
- **Use common-mode cancellation:** Interferometers eliminate amplitude but retain timing.
- **Let the environment justify it:** Inconsistent coupling and absorptive spaces prevent amplitude from being a reliable cue.
- **Let culture reinforce it:** Doll communication prefers pulses, phase interplay, and timing motifs.