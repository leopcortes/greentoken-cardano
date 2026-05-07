import { useId } from 'react';
import type { CSSProperties } from 'react';
import type { BottleInvalid, BottleKind, BottleSize, BottleTint } from '@/lib/types';

interface BottleProps {
  size?: BottleSize;
  tint?: BottleTint;
  kind?: BottleKind;
  invalid?: BottleInvalid;
  style?: CSSProperties;
  className?: string;
}

const HEIGHTS: Record<BottleSize, number> = { S: 56, M: 78, L: 104 };

const TINTS: Record<BottleTint, { body: string; stroke: string; label: string }> = {
  clear: { body: 'rgba(220, 252, 231, 0.55)', stroke: '#16a34a', label: '#16a34a' },
  green: { body: 'rgba(134, 239, 172, 0.85)', stroke: '#15803d', label: '#15803d' },
  blue:  { body: 'rgba(147, 197, 253, 0.85)', stroke: '#1d4ed8', label: '#1d4ed8' },
  amber: { body: 'rgba(253, 230, 138, 0.85)', stroke: '#a16207', label: '#a16207' },
};

export function Bottle({
  size = 'M',
  tint = 'clear',
  kind = 'PET',
  invalid = null,
  style,
  className,
}: BottleProps) {
  const gradId = useId();
  const h = HEIGHTS[size];
  const w = h * 0.42;
  const t = TINTS[tint];

  if (invalid === 'can') {
    return (
      <svg width={w * 1.1} height={h} viewBox="0 0 44 104" style={style} className={className}>
        <defs>
          <linearGradient id={`canG-${gradId}`} x1="0" x2="1">
            <stop offset="0" stopColor="#cbd5e1" />
            <stop offset="0.5" stopColor="#f1f5f9" />
            <stop offset="1" stopColor="#94a3b8" />
          </linearGradient>
        </defs>
        <rect x="6" y="8" width="32" height="88" rx="3" fill={`url(#canG-${gradId})`} stroke="#64748b" strokeWidth="1" />
        <rect x="6" y="14" width="32" height="3" fill="#64748b" opacity="0.4" />
        <rect x="6" y="86" width="32" height="3" fill="#64748b" opacity="0.4" />
        <rect x="11" y="40" width="22" height="22" fill="#dc2626" rx="2" />
        <text x="22" y="55" textAnchor="middle" fontSize="9" fontWeight="700" fill="#fff" fontFamily="system-ui">soda</text>
      </svg>
    );
  }

  if (invalid === 'glass') {
    return (
      <svg width={w * 1.05} height={h} viewBox="0 0 46 104" style={style} className={className}>
        <path
          d="M16 4 L30 4 L32 14 L34 28 L34 92 Q34 98 28 98 L18 98 Q12 98 12 92 L12 28 L14 14 Z"
          fill="rgba(74, 222, 128, 0.35)"
          stroke="#166534"
          strokeWidth="1.2"
        />
        <path d="M16 4 L30 4 L32 14 L14 14 Z" fill="rgba(20,83,45,0.2)" stroke="#166534" strokeWidth="1.2" />
        <ellipse cx="20" cy="40" rx="3" ry="6" fill="rgba(255,255,255,0.4)" />
      </svg>
    );
  }

  return (
    <svg width={w} height={h} viewBox="0 0 44 110" style={style} className={className}>
      <defs>
        <linearGradient id={`bg-${gradId}`} x1="0" x2="1">
          <stop offset="0" stopColor={t.body} stopOpacity="0.9" />
          <stop offset="0.45" stopColor={t.body} stopOpacity="0.7" />
          <stop offset="1" stopColor={t.body} stopOpacity="0.95" />
        </linearGradient>
      </defs>
      <rect x="16" y="2" width="12" height="6" rx="1.5" fill={t.stroke} />
      <rect x="15" y="8" width="14" height="4" rx="1" fill={t.stroke} opacity="0.85" />
      <path d="M17 12 L27 12 L28 18 L16 18 Z" fill="none" stroke={t.stroke} strokeWidth="1.2" />
      <path
        d="M16 18 L28 18 L32 28 L33 38 L33 92 Q33 102 25 102 L19 102 Q11 102 11 92 L11 38 L12 28 Z"
        fill={`url(#bg-${gradId})`}
        stroke={t.stroke}
        strokeWidth="1.3"
        strokeLinejoin="round"
      />
      <path d="M14 28 L14 88 Q14 96 18 98" fill="none" stroke="rgba(255,255,255,0.7)" strokeWidth="1.5" strokeLinecap="round" />
      <rect x="11.5" y="50" width="21.5" height="22" fill="rgba(255,255,255,0.55)" stroke={t.label} strokeWidth="0.6" />
      <text x="22.25" y="60" textAnchor="middle" fontSize="6" fontWeight="700" fill={t.label} fontFamily="system-ui" letterSpacing="0.5">{kind}</text>
      <text x="22.25" y="68" textAnchor="middle" fontSize="4.5" fill={t.label} fontFamily="system-ui">
        {size === 'S' ? '300ml' : size === 'L' ? '2L' : '600ml'}
      </text>
      <ellipse cx="22" cy="100" rx="10" ry="2" fill={t.stroke} opacity="0.3" />
    </svg>
  );
}
