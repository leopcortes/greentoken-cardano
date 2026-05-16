import { InventoryBottle } from '@/components/InventoryBottle';
import { TooltipProvider } from '@/components/ui/tooltip';
import { useStation } from './StationContext';

export function InventoryPage() {
  const { inventory, dragging, scanning, creating, onPickStart } = useStation();
  // Bloqueia o inventario inteiro enquanto a IA esta validando ou o backend
  // ainda esta criando a garrafa (POST /bottles em voo). Sem isso, drops
  // rapidos disparam POSTs concorrentes que podem (a) gerar bottle_id_text
  // duplicado e (b) extrapolar a capacidade do container.
  const validating = scanning || creating;
  const disabled = !!dragging || validating;

  return (
    <TooltipProvider delayDuration={150} skipDelayDuration={0}>
      <div className="gt-card p-[18px] flex flex-col min-h-0 h-full">
        <div className="flex justify-between items-baseline mb-[6px]">
          <div className="gt-eyebrow">Inventário</div>
          <span className="gt-chip gt-chip--ghost">{inventory.length} itens</span>
        </div>
        <div className="text-[11px] mb-[14px] text-ink-3">
          Arraste uma garrafa para o container. Itens não-plástico são rejeitados pela IA.
        </div>
        <div
          className="relative flex-1 min-h-0 grid grid-cols-4 auto-rows-[minmax(96px,auto)] gap-[6px] content-start overflow-auto px-[16px] py-[10px] bg-bg rounded-xl border border-dashed border-line gt-no-scrollbar transition-opacity"
          style={{ opacity: validating ? 0.5 : 1 }}
          aria-busy={validating}
        >
          {inventory.map((b) => (
            <div key={b.id} className="flex justify-center items-end min-h-[96px] p-1">
              <InventoryBottle b={b} disabled={disabled} onPickStart={onPickStart} />
            </div>
          ))}
          {validating && (
            <div
              className="absolute inset-0 z-10 cursor-not-allowed rounded-xl"
              style={{ pointerEvents: 'all' }}
              aria-hidden
            />
          )}
        </div>
      </div>
    </TooltipProvider>
  );
}
