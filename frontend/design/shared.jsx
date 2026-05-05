/* ============ SHARED — bottle SVGs, data, helpers ============
 * Exposes: window.Bottle, window.bottleData, window.gtHelpers
 * Bottle component renders an isolated PET bottle with size + tint variants.
 */

const Bottle = ({ size = 'M', tint = 'clear', kind = 'PET', invalid = false, style = {}, className = '', ...rest }) => {
  // height in px; widths derived
  const heights = { S: 56, M: 78, L: 104 };
  const h = heights[size] || 78;
  const w = h * 0.42;

  // body fill by tint
  const tints = {
    clear:   { body: 'rgba(220, 252, 231, 0.55)', stroke: '#16a34a', label: '#16a34a' },
    green:   { body: 'rgba(134, 239, 172, 0.85)', stroke: '#15803d', label: '#15803d' },
    blue:    { body: 'rgba(147, 197, 253, 0.85)', stroke: '#1d4ed8', label: '#1d4ed8' },
    amber:   { body: 'rgba(253, 230, 138, 0.85)', stroke: '#a16207', label: '#a16207' },
  };
  const t = tints[tint] || tints.clear;

  // invalid items override — render as can or glass shard
  if (invalid === 'can') {
    return (
      <svg width={w*1.1} height={h} viewBox="0 0 44 104" style={style} className={className} {...rest}>
        <defs>
          <linearGradient id={`canG-${kind}`} x1="0" x2="1">
            <stop offset="0" stopColor="#cbd5e1"/>
            <stop offset="0.5" stopColor="#f1f5f9"/>
            <stop offset="1" stopColor="#94a3b8"/>
          </linearGradient>
        </defs>
        <rect x="6" y="8" width="32" height="88" rx="3" fill={`url(#canG-${kind})`} stroke="#64748b" strokeWidth="1"/>
        <rect x="6" y="14" width="32" height="3" fill="#64748b" opacity="0.4"/>
        <rect x="6" y="86" width="32" height="3" fill="#64748b" opacity="0.4"/>
        <rect x="11" y="40" width="22" height="22" fill="#dc2626" rx="2"/>
        <text x="22" y="55" textAnchor="middle" fontSize="9" fontWeight="700" fill="#fff" fontFamily="system-ui">soda</text>
      </svg>
    );
  }
  if (invalid === 'glass') {
    return (
      <svg width={w*1.05} height={h} viewBox="0 0 46 104" style={style} className={className} {...rest}>
        <path d="M16 4 L30 4 L32 14 L34 28 L34 92 Q34 98 28 98 L18 98 Q12 98 12 92 L12 28 L14 14 Z"
          fill="rgba(74, 222, 128, 0.35)" stroke="#166534" strokeWidth="1.2"/>
        <path d="M16 4 L30 4 L32 14 L14 14 Z" fill="rgba(20,83,45,0.2)" stroke="#166534" strokeWidth="1.2"/>
        <ellipse cx="20" cy="40" rx="3" ry="6" fill="rgba(255,255,255,0.4)"/>
      </svg>
    );
  }

  // base PET shape — proportions follow size
  return (
    <svg width={w} height={h} viewBox="0 0 44 110" style={style} className={className} {...rest}>
      <defs>
        <linearGradient id={`bg-${tint}-${size}`} x1="0" x2="1">
          <stop offset="0" stopColor={t.body} stopOpacity="0.9"/>
          <stop offset="0.45" stopColor={t.body} stopOpacity="0.7"/>
          <stop offset="1" stopColor={t.body} stopOpacity="0.95"/>
        </linearGradient>
      </defs>
      {/* cap */}
      <rect x="16" y="2"  width="12" height="6" rx="1.5" fill={t.stroke}/>
      <rect x="15" y="8"  width="14" height="4" rx="1"   fill={t.stroke} opacity="0.85"/>
      {/* neck */}
      <path d="M17 12 L27 12 L28 18 L16 18 Z" fill="none" stroke={t.stroke} strokeWidth="1.2"/>
      {/* shoulder + body */}
      <path
        d="M16 18 L28 18 L32 28 L33 38 L33 92 Q33 102 25 102 L19 102 Q11 102 11 92 L11 38 L12 28 Z"
        fill={`url(#bg-${tint}-${size})`}
        stroke={t.stroke}
        strokeWidth="1.3"
        strokeLinejoin="round"
      />
      {/* highlight */}
      <path d="M14 28 L14 88 Q14 96 18 98" fill="none" stroke="rgba(255,255,255,0.7)" strokeWidth="1.5" strokeLinecap="round"/>
      {/* label band */}
      <rect x="11.5" y="50" width="21.5" height="22" fill="rgba(255,255,255,0.55)" stroke={t.label} strokeWidth="0.6"/>
      <text x="22.25" y="60" textAnchor="middle" fontSize="6" fontWeight="700" fill={t.label} fontFamily="system-ui" letterSpacing="0.5">{kind}</text>
      <text x="22.25" y="68" textAnchor="middle" fontSize="4.5" fill={t.label} fontFamily="system-ui">{size === 'S' ? '300ml' : size === 'L' ? '2L' : '600ml'}</text>
      {/* base ridge */}
      <ellipse cx="22" cy="100" rx="10" ry="2" fill={t.stroke} opacity="0.3"/>
    </svg>
  );
};

/* deterministic, fixed 20-item inventory */
function buildInventory(seed = 7) {
  const sizes = ['S', 'M', 'L'];
  const tints = ['clear', 'clear', 'green', 'blue', 'amber', 'clear'];
  const kinds = ['PET', 'PET', 'PET', 'HDPE', 'PET'];
  const invalids = [null, null, null, null, null, null, null, null, null, 'can', 'glass'];
  let s = seed;
  const rand = () => { s = (s * 9301 + 49297) % 233280; return s / 233280; };
  const list = [];
  for (let i = 0; i < 20; i++) {
    const inv = invalids[Math.floor(rand() * invalids.length)];
    list.push({
      id: `b-${i}`,
      size: sizes[Math.floor(rand() * sizes.length)],
      tint: tints[Math.floor(rand() * tints.length)],
      kind: inv ? (inv === 'can' ? 'AL' : 'GLASS') : kinds[Math.floor(rand() * kinds.length)],
      invalid: inv,
      rot: (rand() * 24 - 12).toFixed(1) + 'deg',
    });
  }
  return list;
}

/* generate a single replacement bottle (random props, unique id) */
function makeReplacementBottle() {
  const sizes = ['S', 'M', 'L'];
  const tints = ['clear', 'clear', 'green', 'blue', 'amber', 'clear'];
  const kinds = ['PET', 'PET', 'PET', 'HDPE'];
  const invalids = [null, null, null, null, null, null, null, null, null, 'can', 'glass'];
  const rand = Math.random;
  const inv = invalids[Math.floor(rand() * invalids.length)];
  return {
    id: `b-r-${Date.now()}-${Math.floor(rand()*9999)}`,
    size: sizes[Math.floor(rand() * sizes.length)],
    tint: tints[Math.floor(rand() * tints.length)],
    kind: inv ? (inv === 'can' ? 'AL' : 'GLASS') : kinds[Math.floor(rand() * kinds.length)],
    invalid: inv,
    rot: (rand() * 24 - 12).toFixed(1) + 'deg',
  };
}

const STAGES = [
  { id: 'inserted',  label: 'Inserida',     reward: 10, short: 'Insert' },
  { id: 'compacted', label: 'Compactada',   reward: 3,  short: 'Compact' },
  { id: 'collected', label: 'Coletada',     reward: 7,  short: 'Collect' },
  { id: 'atstation', label: 'Na estação',   reward: 10, short: 'Station' },
  { id: 'shredded',  label: 'Triturada',    reward: 20, short: 'Shred' },
];

function fakeTxHash() {
  const hex = '0123456789abcdef';
  let h = '';
  for (let i = 0; i < 64; i++) h += hex[Math.floor(Math.random() * 16)];
  return h;
}

function truncMid(s, head = 8, tail = 6) {
  if (!s || s.length <= head + tail + 1) return s;
  return s.slice(0, head) + '…' + s.slice(-tail);
}

const helpers = { STAGES, fakeTxHash, truncMid, buildInventory, makeReplacementBottle };

Object.assign(window, { Bottle, gtHelpers: helpers });
