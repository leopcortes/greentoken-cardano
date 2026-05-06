import { Container } from '@/components/Container';
import { Button } from '@/components/ui/button';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { useStation } from './StationContext';
import { STAGES } from './helpers';
import { CONTAINER_STATUS_LABELS, t } from '@/lib/labels';

const STATUS_CHIP: Record<string, string> = {
  active: 'bg-green-100 border-green-200 text-green-800',
  ready_for_collection: 'bg-orange-100 border-orange-200 text-orange-800',
  in_route: 'bg-blue-100 border-blue-200 text-blue-800',
  maintenance: 'bg-gray-100 border-gray-200 text-gray-700',
};

export function CurrentContainerPage() {
  const {
    binRef, dropArmed, lidOpen, scanning, reject, fillPct, crushed, activeStage,
    setFillPct, setCrushed,
    containers, currentContainer, currentContainerId, setCurrentContainerId,
    setContainers,
  } = useStation();

  const collectReady = fillPct >= 90;
  const handleCollect = () => {
    setFillPct(0);
    setCrushed(0);
    if (currentContainer) {
      setContainers((list) =>
        list.map((c) =>
          c.id === currentContainer.id ? { ...c, current_volume_liters: 0 } : c,
        ),
      );
    }
  };

  const status = currentContainer?.status ?? 'maintenance';
  const statusClass = STATUS_CHIP[status] ?? STATUS_CHIP.maintenance;

  const capacity = currentContainer?.capacity_liters ?? 0;
  const currentVolume = capacity > 0 ? (fillPct / 100) * capacity : 0;

  return (
    <div
      className="relative h-full flex flex-col items-center justify-center rounded-xl border border-dashed border-line p-6"
      style={{
        background: 'radial-gradient(ellipse at 50% 70%, rgba(22,163,74,0.06), transparent 60%)',
      }}
    >
      <div className="absolute top-[18px] left-[22px] right-[22px] flex justify-between items-start gap-3">
        <div className="min-w-0 flex-1">
          <div className="gt-eyebrow truncate">
            {currentContainer?.name ?? 'Nenhum'}
          </div>
          {currentContainer?.location_name && (
            <div className="text-[10px] text-ink-4 mt-0.5 truncate">
              {currentContainer.location_name}
            </div>
          )}
        </div>
        <div className="flex items-center gap-2 flex-none">
          <span
            className={`inline-flex items-center gap-1 px-2 py-[3px] rounded-full text-[11px] font-semibold border ${statusClass}`}
          >
            {status === 'active' && <span className="gt-pulse mr-0.5" />}
            {t(CONTAINER_STATUS_LABELS, status)}
          </span>
          <Select
            value={currentContainerId ?? undefined}
            onValueChange={(v) => setCurrentContainerId(v)}
          >
            <SelectTrigger className="h-8 w-[170px] text-[11px]">
              <SelectValue placeholder="Selecionar" />
            </SelectTrigger>
            <SelectContent>
              {containers.length === 0 ? (
                <div className="px-3 py-4 text-xs text-center text-muted-foreground">
                  Nenhum container
                </div>
              ) : (
                containers.map((c) => (
                  <SelectItem key={c.id} value={c.id} className="text-[11px]">
                    {c.name}
                  </SelectItem>
                ))
              )}
            </SelectContent>
          </Select>
        </div>
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
          'Arraste uma garrafa do inventário.'
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
            <span className="mono">{currentVolume.toFixed(2)} L / {capacity.toFixed(2)} L</span>
            <span>100%</span>
          </div>
        </div>
        {collectReady && (
          <Button
            onClick={handleCollect}
            size="sm"
            className="ml-4 flex-none bg-orange-200 text-orange-800 hover:bg-orange-300 text-[11px] font-semibold px-3 h-8 whitespace-nowrap"
            style={{ boxShadow: '0 2px 6px rgba(22,163,74,0.3)' }}
          >
            Coletar
          </Button>
        )}
      </div>
    </div>
  );
}
