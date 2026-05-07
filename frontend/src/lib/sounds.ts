// Efeitos sonoros sintetizados via Web Audio API. Evita assets binários e roda
// dentro do gesto do usuário (drop = mouseup), satisfazendo o autoplay policy.

let ctx: AudioContext | null = null;

function getCtx(): AudioContext | null {
  if (typeof window === 'undefined') return null;
  if (ctx) return ctx;
  const Ctor = window.AudioContext ?? (window as unknown as { webkitAudioContext?: typeof AudioContext }).webkitAudioContext;
  if (!Ctor) return null;
  ctx = new Ctor();
  return ctx;
}

function resume(c: AudioContext) {
  if (c.state === 'suspended') void c.resume();
}

/**
 * Plástico caindo: ruído branco filtrado (band-pass alto) com envelope curto
 * + um thud sub-grave breve. Soa como uma garrafa PET batendo numa superfície.
 */
export function playBottleDrop(volume = 0.55) {
  const c = getCtx();
  if (!c) return;
  resume(c);
  const now = c.currentTime;

  // Buffer de ruído branco (~250ms)
  const noiseLen = Math.floor(c.sampleRate * 0.25);
  const buffer = c.createBuffer(1, noiseLen, c.sampleRate);
  const data = buffer.getChannelData(0);
  for (let i = 0; i < noiseLen; i++) data[i] = Math.random() * 2 - 1;

  const noise = c.createBufferSource();
  noise.buffer = buffer;

  const bandPass = c.createBiquadFilter();
  bandPass.type = 'bandpass';
  bandPass.frequency.value = 1800;
  bandPass.Q.value = 0.9;

  const noiseGain = c.createGain();
  noiseGain.gain.setValueAtTime(0, now);
  noiseGain.gain.linearRampToValueAtTime(volume * 0.7, now + 0.005);
  noiseGain.gain.exponentialRampToValueAtTime(0.001, now + 0.18);

  noise.connect(bandPass).connect(noiseGain).connect(c.destination);
  noise.start(now);
  noise.stop(now + 0.25);

  // Thud grave: oscilador low sweep
  const thud = c.createOscillator();
  thud.type = 'sine';
  thud.frequency.setValueAtTime(140, now);
  thud.frequency.exponentialRampToValueAtTime(55, now + 0.15);

  const thudGain = c.createGain();
  thudGain.gain.setValueAtTime(0, now);
  thudGain.gain.linearRampToValueAtTime(volume * 0.9, now + 0.01);
  thudGain.gain.exponentialRampToValueAtTime(0.001, now + 0.2);

  thud.connect(thudGain).connect(c.destination);
  thud.start(now);
  thud.stop(now + 0.22);
}

/**
 * Recompensa de moeda: dois pings curtos em intervalo de quinta justa
 * (880Hz → 1318Hz), com envelope rápido. Soa como "ka-ching" leve.
 */
export function playCoinReward(volume = 0.4) {
  const c = getCtx();
  if (!c) return;
  resume(c);
  const now = c.currentTime;

  const ping = (freq: number, startOffset: number, dur = 0.18) => {
    const osc = c.createOscillator();
    osc.type = 'triangle';
    osc.frequency.value = freq;

    const gain = c.createGain();
    const t0 = now + startOffset;
    gain.gain.setValueAtTime(0, t0);
    gain.gain.linearRampToValueAtTime(volume, t0 + 0.005);
    gain.gain.exponentialRampToValueAtTime(0.001, t0 + dur);

    osc.connect(gain).connect(c.destination);
    osc.start(t0);
    osc.stop(t0 + dur + 0.02);
  };

  ping(880, 0);
  ping(1318.5, 0.07);
}
