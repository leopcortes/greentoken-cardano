import { Link } from 'react-router-dom';

export function TopBar() {
  return (
    <div className="flex items-center justify-between gap-4">
      <div className="flex items-center gap-3">
        <div
          className="w-9 h-9 rounded-[10px] bg-gt-600 text-white flex items-center justify-center font-extrabold text-base tracking-[-0.04em]"
          style={{ boxShadow: '0 4px 12px rgba(22,163,74,0.4)' }}
        >
          ₲
        </div>
        <div>
          <div className="text-sm font-bold tracking-[-0.01em]">Greentoken Station</div>
          <div className="text-[11px] text-ink-3">Container de reciclagem na blockchain Cardano</div>
        </div>
      </div>

      <div className="flex gap-[10px] items-center">
        <span className="gt-chip gt-chip--cdn">Cardano · preprod</span>
        <Link
          to="/dashboard"
          className="inline-flex items-center gap-[6px] px-3 py-[6px] rounded-lg bg-ink text-bg-card text-[11px] font-semibold tracking-[0.01em] no-underline border border-ink transition-opacity hover:opacity-85"
        >
          Dashboard
          <svg width="11" height="11" viewBox="0 0 12 12" fill="none">
            <path d="M3 3 L9 3 L9 9 M9 3 L3 9" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round" />
          </svg>
        </Link>
      </div>
    </div>
  );
}
