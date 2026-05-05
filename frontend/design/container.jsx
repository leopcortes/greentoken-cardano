/* ============ Container — green recycling bin SVG with animated lid + scan ============
 * Props:
 *   open: boolean (lid)
 *   scanning: boolean (AI scanline overlay)
 *   reject: boolean (red flash)
 *   fillPct: 0..100 (volume indicator on side)
 *   crushedCount: number (animated bottle crushing inside)
 *   variant: 'studio' | 'pipeline' | 'player'
 */

const Container = ({ open = false, scanning = false, reject = false, fillPct = 32, crushedCount = 0, variant = 'studio' }) => {
  // palette varies subtly per variant
  const palette = variant === 'player'
    ? { body1: '#16a34a', body2: '#15803d', edge: '#052e16', interior: '#031910', stroke: '#052e16' }
    : { body1: '#22c55e', body2: '#16a34a', edge: '#14532d', interior: '#0a2e1c', stroke: '#14532d' };

  return (
    <svg viewBox="0 0 320 380" width="100%" height="100%" style={{ overflow: 'visible', display: 'block' }}>
      <defs>
        <linearGradient id="bin-body" x1="0" x2="1">
          <stop offset="0" stopColor={palette.body1}/>
          <stop offset="0.55" stopColor={palette.body2}/>
          <stop offset="1" stopColor={palette.body1}/>
        </linearGradient>
        <linearGradient id="bin-shadow" x1="0" x2="0" y1="0" y2="1">
          <stop offset="0" stopColor="rgba(0,0,0,0)"/>
          <stop offset="1" stopColor="rgba(0,0,0,0.18)"/>
        </linearGradient>
        <clipPath id="bin-interior-clip">
          <path d="M62 96 L258 96 L246 348 L74 348 Z"/>
        </clipPath>
        <linearGradient id="scan-grad" x1="0" x2="0" y1="0" y2="1">
          <stop offset="0"   stopColor="rgba(34,197,94,0)"/>
          <stop offset="0.5" stopColor="rgba(34,197,94,0.55)"/>
          <stop offset="1"   stopColor="rgba(34,197,94,0)"/>
        </linearGradient>
      </defs>

      {/* ground shadow */}
      <ellipse cx="160" cy="368" rx="120" ry="10" fill="rgba(0,0,0,0.18)"/>

      {/* body */}
      <path
        d="M62 96 L258 96 L246 348 Q244 360 232 360 L88 360 Q76 360 74 348 Z"
        fill="url(#bin-body)"
        stroke={palette.stroke}
        strokeWidth="2.5"
        strokeLinejoin="round"
      />

      {/* vertical ribs */}
      {[0, 1, 2, 3].map(i => (
        <line key={i}
          x1={92 + i * 45} y1="100"
          x2={94 + i * 45} y2="356"
          stroke={palette.edge} strokeOpacity="0.25" strokeWidth="1.5"
        />
      ))}

      {/* interior (the dark mouth — visible when lid is open) */}
      <g clipPath="url(#bin-interior-clip)">
        <path
          d="M62 96 L258 96 L246 348 L74 348 Z"
          fill={palette.interior}
          opacity={open ? 1 : 0.35}
          style={{ transition: 'opacity 320ms ease' }}
        />
        {/* compacted layer at bottom — grows with crushedCount */}
        <rect
          x="68" y={Math.max(180, 348 - Math.min(crushedCount, 12) * 12)}
          width="184"
          height={Math.min(crushedCount, 12) * 12}
          fill={palette.body2}
          opacity="0.55"
          style={{ transition: 'all 600ms cubic-bezier(.4,1.2,.6,1)' }}
        />
        {/* AI scan line overlay */}
        {scanning && (
          <rect
            x="62" y="96" width="196" height="60"
            fill="url(#scan-grad)"
            style={{ animation: 'gt-scan 900ms ease-in-out' }}
          />
        )}
      </g>

      {/* mouth rim (front) */}
      <rect x="60" y="92" width="200" height="10" rx="3" fill={palette.edge}/>

      {/* recycling glyph on body */}
      <g transform="translate(160 230)" opacity="0.92">
        <circle r="34" fill="rgba(255,255,255,0.10)"/>
        <g fill="#ffffff">
          <path d="M -22 6 L -10 -14 L 2 -8 L -8 12 Z M 2 -8 L 8 -10 L 4 -22 L -14 -18 Z"/>
          <path transform="rotate(120)" d="M -22 6 L -10 -14 L 2 -8 L -8 12 Z M 2 -8 L 8 -10 L 4 -22 L -14 -18 Z"/>
          <path transform="rotate(240)" d="M -22 6 L -10 -14 L 2 -8 L -8 12 Z M 2 -8 L 8 -10 L 4 -22 L -14 -18 Z"/>
        </g>
      </g>

      {/* fill indicator strip on right — removed (volume bar lives below the bin) */}

      {/* lid — pivots at the back-left hinge */}
      <g style={{
        transformOrigin: '70px 92px',
        transform: open ? 'rotate(-58deg)' : 'rotate(0deg)',
        transition: 'transform 380ms cubic-bezier(.4,1.4,.5,1)',
      }}>
        {/* lid back face (visible when tilted) */}
        <path
          d="M58 78 L262 78 L266 92 L54 92 Z"
          fill={palette.body2}
          stroke={palette.stroke}
          strokeWidth="2.2"
          strokeLinejoin="round"
        />
        {/* lid top */}
        <path
          d="M62 64 L258 64 Q266 64 266 72 L266 78 L54 78 L54 72 Q54 64 62 64 Z"
          fill="url(#bin-body)"
          stroke={palette.stroke}
          strokeWidth="2.2"
          strokeLinejoin="round"
        />
        {/* slot */}
        <rect x="120" y="68" width="80" height="6" rx="3" fill={palette.interior}/>
        {/* hinge dots */}
        <circle cx="70" cy="92" r="2.5" fill={palette.edge}/>
        <circle cx="250" cy="92" r="2.5" fill={palette.edge}/>
      </g>

      {/* reject flash */}
      {reject && (
        <rect x="58" y="58" width="204" height="304" rx="6" fill="none"
              stroke="#dc2626" strokeWidth="3" opacity="0.85"
              style={{ animation: 'gt-shake 300ms ease' }}/>
      )}
    </svg>
  );
};

window.Container = Container;
