import { Bottle } from '@/components/Bottle';
import { useStation } from './StationContext';
import { kindLabelOf, materialLabelOf, sizeLabelOf, tintLabelOf } from './helpers';

type NodeStatus = 'pending' | 'active' | 'done' | 'failed';

const PALETTE: Record<NodeStatus, { bg: string; bd: string; ic: string; tx: string }> = {
  pending: { bg: 'var(--bg-elev)', bd: 'var(--line)',   ic: 'var(--ink-4)', tx: 'var(--ink-4)' },
  active:  { bg: 'white',          bd: 'var(--gt-500)', ic: 'var(--gt-700)', tx: 'var(--ink)'   },
  done:    { bg: 'var(--gt-600)',  bd: 'var(--gt-600)', ic: 'white',         tx: 'var(--ink)'   },
  failed:  { bg: 'var(--err)',     bd: 'var(--err)',    ic: 'white',         tx: 'var(--err)'   },
};

function StatusNode({ label, status }: { label: string; status: NodeStatus }) {
  const s = PALETTE[status];
  const popped = status === 'active' || status === 'done';
  return (
    <div className="flex flex-col items-center gap-[6px] flex-none min-w-[60px]">
      <div
        className="w-[30px] h-[30px] rounded-full flex items-center justify-center relative border-[1.5px] transition-all duration-[280ms]"
        style={{
          background: s.bg,
          borderColor: s.bd,
          boxShadow: status === 'active' ? '0 0 0 4px rgba(34,197,94,0.15)' : 'none',
          animation: popped ? 'gt-node-pop 460ms ease' : 'none',
        }}
      >
        {status === 'done' && (
          <svg width="13" height="13" viewBox="0 0 14 14">
            <path d="M3 7 L6 10 L11 4" stroke={s.ic} strokeWidth="2.2" fill="none" strokeLinecap="round" strokeLinejoin="round" />
          </svg>
        )}
        {status === 'failed' && (
          <svg width="11" height="11" viewBox="0 0 14 14">
            <path d="M3 3 L11 11 M11 3 L3 11" stroke={s.ic} strokeWidth="2.2" strokeLinecap="round" />
          </svg>
        )}
        {status === 'active' && <span className="gt-pulse" />}
      </div>
      <div
        className="text-[11px] leading-tight text-center"
        style={{ color: s.tx, fontWeight: status === 'pending' ? 500 : 600 }}
      >
        {label}
      </div>
    </div>
  );
}

function StatusArrow({ active }: { active: boolean }) {
  const color = active ? 'var(--gt-500)' : 'var(--line)';
  return (
    <div className="flex-1 flex items-center justify-center h-[30px] relative">
      <div
        className="w-full h-[2px] transition-colors duration-[320ms]"
        style={{ background: color }}
      />
      <svg width="10" height="10" viewBox="0 0 10 10" className="absolute -right-[2px]">
        <path
          d="M2 2 L7 5 L2 8"
          fill="none"
          stroke={color}
          strokeWidth="1.6"
          strokeLinecap="round"
          strokeLinejoin="round"
        />
      </svg>
    </div>
  );
}

export function CurrentBottlePage() {
  const { currentBottle, aiResult, activeStage, completed } = useStation();

  const aiStatus: NodeStatus =
    aiResult === 'validating' ? 'active' :
    aiResult === 'accepted'   ? 'done' :
    aiResult === 'rejected'   ? 'failed' :
                                 'pending';

  const insertedDone = completed.has('inserted');
  const insertedActive = activeStage === 0;
  const insertedStatus: NodeStatus =
    aiResult === 'rejected' ? 'pending' :
    insertedDone            ? 'done' :
    insertedActive          ? 'active' :
                              'pending';

  const compactedDone = completed.has('compacted');
  const compactedActive = activeStage === 1;
  const compactedStatus: NodeStatus =
    aiResult === 'rejected' ? 'pending' :
    compactedDone           ? 'done' :
    compactedActive         ? 'active' :
                              'pending';

  const accepted = currentBottle && !currentBottle.invalid;

  return (
    <div className="gt-card p-[18px]">
      <div className="flex justify-between items-baseline mb-[14px]">
        <div className="gt-eyebrow">Garrafa atual</div>
        {currentBottle && (
          <span className="mono text-[10px] text-ink-4">#{currentBottle.id.slice(-6)}</span>
        )}
      </div>

      <div
        className="flex gap-[14px] items-center p-[8px] rounded-xl border transition-[background,border-color] duration-300 min-h-[110px]"
        style={{
          background: currentBottle ? (accepted ? 'var(--gt-50)' : 'var(--err-soft)') : 'var(--bg)',
          borderColor: currentBottle ? (accepted ? 'var(--gt-200)' : '#fecaca') : 'var(--line)',
        }}
      >
        <div className="flex-none flex items-center justify-center w-[70px] h-[90px]">
          {currentBottle ? (
            <Bottle size={currentBottle.size} tint={currentBottle.tint} kind={currentBottle.kind} invalid={currentBottle.invalid} />
          ) : (
            <div className="w-9 h-[70px] rounded-lg border-[1.5px] border-dashed border-line flex items-center justify-center text-ink-4 text-lg">
              ?
            </div>
          )}
        </div>
        <div className="flex-1 min-w-0">
          {currentBottle ? (
            <>
              <div className="text-base font-bold text-ink tracking-[-0.01em]">
                {kindLabelOf(currentBottle)} · {sizeLabelOf(currentBottle.size)}
              </div>
              <div className="text-[11px] text-ink-3 mt-1 leading-[1.5]">
                Cor: {tintLabelOf(currentBottle.tint)}<br />
                Material: {materialLabelOf(currentBottle)}
              </div>
            </>
          ) : (
            <>
              <div className="text-sm font-semibold text-ink-2">Aguardando inserção</div>
              <div className="text-[11px] text-ink-3 mt-1 leading-[1.5]">
                Arraste uma garrafa do inventário para o container ao lado.
              </div>
            </>
          )}
        </div>
      </div>

      <div className="mt-[14px] flex items-start justify-between px-4 py-1">
        <StatusNode label="Validada"   status={aiStatus} />
        <StatusArrow active={aiStatus === 'done'} />
        <StatusNode label="Inserida"   status={insertedStatus} />
        <StatusArrow active={insertedStatus === 'done'} />
        <StatusNode label="Compactada" status={compactedStatus} />
      </div>

      <div className="mt-2 pt-2 border-t border-line text-[10px] text-ink-4 leading-[1.5]">
        Próximas etapas acontecem fora do container e renderão até{' '}
        <span className="text-gt-700 font-bold">+37 ₲</span> a mais.
      </div>
    </div>
  );
}
