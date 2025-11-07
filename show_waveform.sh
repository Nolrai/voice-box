#!/usr/bin/env python3

# Usage: ./show_waveform.sh filename.wav

import sys
import numpy as np
import matplotlib.pyplot as plt
from scipy.io import wavfile

sr, data = wavfile.read(sys.argv[1])
if data.ndim>1: data = data.mean(axis=1)
t = np.arange(len(data))/sr

plt.figure(figsize=(10,6))
plt.subplot(2,1,1); plt.plot(t, data); plt.title('waveform'); plt.xlabel('s')
plt.subplot(2,1,2); plt.specgram(data, NFFT=1024, Fs=sr, noverlap=512, cmap='magma'); plt.title('spectrogram'); plt.ylim(0, 4000)
plt.tight_layout(); plt.show()

