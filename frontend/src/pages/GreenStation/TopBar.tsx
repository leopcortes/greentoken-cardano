import { Link } from 'react-router-dom';
import { useStation } from './StationContext';

export function TopBar() {
  const { bottlesProcessed } = useStation();
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
        <span className="gt-chip gt-chip--ghost">
          <span className="mono">{bottlesProcessed}</span> hoje
        </span>
        <span className="gt-chip gt-chip--cdn">Cardano · preprod</span>
        <Link
          to="/wallet"
          className="inline-flex items-center gap-[6px] px-3 py-[6px] rounded-lg border border-line text-ink-2 text-[11px] font-semibold no-underline hover:bg-bg-elev transition-colors"
        >
          Minha carteira
        </Link>
      </div>
    </div>
  );
}
