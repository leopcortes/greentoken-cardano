import { Container } from '@/components/Container';
import { Button } from '@/components/ui/button';
import { useStation } from './StationContext';
import { STAGES } from './helpers';

export function CurrentContainerPage() {
  const {
    binRef, dropArmed, lidOpen, scanning, reject, fillPct, crushed, activeStage,
    setFillPct, setCrushed,
  } = useStation();

  const collectReady = fillPct >= 90;
  const handleCollect = () => {
    setFillPct(0);
    setCrushed(0);
  };

  return (
    <div
      className="relative h-full flex flex-col items-center justify-center rounded-xl border border-dashed border-line p-6"
      style={{
        background: 'radial-gradient(ellipse at 50% 70%, rgba(22,163,74,0.06), transparent 60%)',
      }}
    >
      <div className="absolute top-[18px] left-[22px] right-[22px] flex justify-between items-center">
        <span className="gt-eyebrow">Container · Pão de Açúcar 712</span>
        <span className="gt-chip gt-chip--green"><span className="gt-pulse mr-1" />Online</span>
      </div>

      <div
        ref={binRef}
        className="relative w-[320px] h-[380px] mt-[30px] rounded-2xl transition-transform duration-300"
        style={{
          transform: dropArmed ? 'scale(1.04)' : 'scale(1)',
          animation: dropArmed ? 'gt-drop-glow 1.2s ease-in-out infinite' : 'none',
        }}
      >
        <Container open={lidOpen} scanning={scanning} reject={reject} fillPct={fillPct} crushedCount={crushed} />
        {scanning && (
          <div
            className="absolute top-[70px] left-1/2 -translate-x-1/2 px-[10px] py-1 rounded text-[10px] font-semibold tracking-[0.08em] uppercase font-mono"
            style={{
              background: 'rgba(20,83,45,0.92)',
              color: '#bbf7d0',
              border: '1px solid rgba(187,247,208,0.3)',
            }}
          >
            AI · Identificando…
          </div>
        )}
      </div>

      <div className="mt-[18px] text-xs text-ink-3 text-center max-w-[320px]">
        {activeStage >= 0 ? (
          <span><strong className="text-gt-700">Processando…</strong> {STAGES[activeStage]?.label}</span>
        ) : (
          'Arraste uma garrafa do inventário para iniciar.'
        )}
      </div>

<div className='flex mt-[22px] w-full max-w-[460px] items-center'>

      <div className="w-full max-w-[460px]">
        <div className="flex justify-between items-baseline mb-2">
          <span className="gt-eyebrow">Volume do container</span>
          <span
            className="mono text-xs font-bold"
            style={{ color: collectReady ? 'var(--warn)' : 'var(--ink-2)' }}
            >
            {fillPct}%
          </span>
        </div>
        <div className="flex items-center gap-3">
          <div className="relative h-[14px] flex-1 rounded-full bg-bg-elev border border-line overflow-hidden">
            <div
              className="absolute inset-0 rounded-full transition-[width] duration-500"
              style={{
                width: `${fillPct}%`,
                background: collectReady
                ? 'linear-gradient(90deg, var(--gt-500), #f59e0b)'
                : 'linear-gradient(90deg, var(--gt-400), var(--gt-600))',
                boxShadow: 'inset 0 1px 0 rgba(255,255,255,0.35)',
              }}
              />
            <div className="absolute -top-0.5 -bottom-0.5 left-[90%] w-px bg-ink-3 opacity-40" />
          </div>
        </div>
        <div className="flex justify-between mt-[6px] text-[10px] text-ink-4">
          <span>0%</span>
          <span>100%</span>
        </div>
      </div>
        {collectReady && (
          <Button
          onClick={handleCollect}
          size="sm"
          className="ml-4 flex-none bg-orange-200 text-orange-800 hover:bg-orange-300  text-[11px] font-semibold px-3 h-8 whitespace-nowrap"
          style={{ boxShadow: '0 2px 6px rgba(22,163,74,0.3)' }}
          >
              Coletar
            </Button>
          )}
    </div>
          </div>
  );
}
