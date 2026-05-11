import { useEffect, useState } from 'react';
import { Link } from 'react-router-dom';
import { useStation } from './StationContext';

function ProcessingTimerChip({ startedAt }: { startedAt: number }) {
  const [elapsed, setElapsed] = useState(() => Math.floor((Date.now() - startedAt) / 1000));
  useEffect(() => {
    const tick = () => setElapsed(Math.floor((Date.now() - startedAt) / 1000));
    tick();
    const id = window.setInterval(tick, 1000);
    return () => window.clearInterval(id);
  }, [startedAt]);
  return (
    <span
      className="gt-chip"
      style={{
        background: 'var(--warn-soft)',
        borderColor: 'var(--err-soft)',
        color: 'var(--warn)',
      }}
      title="Aguardando confirmação on-chain"
    >
      <svg
        width="11"
        height="11"
        viewBox="0 0 24 24"
        fill="none"
        className="animate-spin mr-1"
        aria-hidden
      >
        <circle cx="12" cy="12" r="9" stroke="currentColor" strokeWidth="3" opacity="0.25" />
        <path d="M21 12a9 9 0 0 0-9-9" stroke="currentColor" strokeWidth="3" strokeLinecap="round" />
      </svg>
      <span>Confirmando</span>
      <span className="mono ml-1">{elapsed}s</span>
    </span>
  );
}

export function TopBar() {
  const { bottlesProcessed, processingStartedAt } = useStation();
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
          <div className="text-md font-bold tracking-[-0.01em] leading-snug">Greentoken Station</div>
          <div className="text-[12px] text-ink-3 leading-tight">Container de reciclagem na blockchain Cardano</div>
        </div>
      </div>

      <div className="flex gap-[10px] items-center">
        {/* {processingStartedAt !== null && <ProcessingTimerChip startedAt={processingStartedAt} />} */}
        <span className="gt-chip gt-chip--ghost">
          <span className="mono">{bottlesProcessed}</span> hoje
        </span>
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
