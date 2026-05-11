import { useEffect, useState } from 'react';
import { useStation } from './StationContext';
import type { InFlightBottle } from './StationContext';

function formatElapsed(ms: number): string {
  const s = Math.max(0, Math.floor(ms / 1000));
  if (s < 60) return `${s}s`;
  const m = Math.floor(s / 60);
  const rem = s % 60;
  return `${m}m${rem.toString().padStart(2, '0')}s`;
}

interface RowProps {
  bottle: InFlightBottle;
  now: number;
}

function PipelineRow({ bottle, now }: RowProps) {
  const insertedDone = bottle.phase === 'compacted';
  const insertedLabel = insertedDone ? 'inserida' : 'inserindo';
  const insertedIcon = insertedDone ? '✓' : '◌';
  const compactedIcon = '◌';
  const elapsed = formatElapsed(now - bottle.enqueuedAt);

  return (
    <div className="flex items-center justify-between gap-3 px-3 py-2 rounded-lg border border-line bg-bg">
      <div className="min-w-0 flex-1">
        <div className="mono text-[11px] font-semibold truncate">{bottle.bottleIdText}</div>
        <div className="text-[10px] text-ink-4 mt-0.5 flex items-center gap-2">
          <span className={insertedDone ? 'text-gt-700' : 'text-amber-600'}>
            {insertedIcon} {insertedLabel}
          </span>
          <span className="text-ink-4">→</span>
          <span className={insertedDone ? 'text-amber-600' : 'text-ink-4'}>
            {compactedIcon} compactando
          </span>
        </div>
      </div>
      <span className="mono text-[10px] text-ink-3 flex-none">{elapsed}</span>
    </div>
  );
}

export function PipelinePanel() {
  const { inFlight } = useStation();
  const [now, setNow] = useState(() => Date.now());

  useEffect(() => {
    if (inFlight.length === 0) return;
    const id = window.setInterval(() => setNow(Date.now()), 1000);
    return () => window.clearInterval(id);
  }, [inFlight.length]);

  return (
    <div className="gt-card p-[14px] flex flex-col min-h-0">
      <div className="flex justify-between items-baseline mb-[8px]">
        <div className="gt-eyebrow">Pipeline on-chain</div>
        <span className="gt-chip gt-chip--ghost">{inFlight.length} em voo</span>
      </div>
      {inFlight.length === 0 ? (
        <div className="text-[11px] text-ink-4 px-1 py-2">
          Sem garrafas em processamento. Arraste do inventário para enfileirar.
        </div>
      ) : (
        <div className="flex flex-col gap-1.5 max-h-[240px] overflow-auto gt-no-scrollbar">
          {inFlight.map((b) => (
            <PipelineRow key={b.backendId} bottle={b} now={now} />
          ))}
        </div>
      )}
    </div>
  );
}
