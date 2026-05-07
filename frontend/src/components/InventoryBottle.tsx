import { useRef } from 'react';
import { Bottle } from './Bottle';
import { Tooltip, TooltipContent, TooltipTrigger } from '@/components/ui/tooltip';
import {
  kindLabelOf,
  materialLabelOf,
  sizeLabelOf,
  tintLabelOf,
} from '@/lib/helpers';
import type { InventoryBottleData } from '@/lib/types';

interface InventoryBottleProps {
  b: InventoryBottleData;
  disabled?: boolean;
  onPickStart: (
    b: InventoryBottleData,
    meta: { offsetX: number; offsetY: number; startX: number; startY: number },
  ) => void;
}

export function InventoryBottle({ b, disabled = false, onPickStart }: InventoryBottleProps) {
  const ref = useRef<HTMLDivElement>(null);

  const handleDown = (e: React.MouseEvent | React.TouchEvent) => {
    if (disabled || !ref.current) return;
    e.preventDefault();
    const rect = ref.current.getBoundingClientRect();
    const point = 'touches' in e ? e.touches[0] : e;
    const offsetX = point.clientX - rect.left;
    const offsetY = point.clientY - rect.top;
    onPickStart(b, { offsetX, offsetY, startX: rect.left, startY: rect.top });
  };

  const idDigits = parseFloat(b.id.replace(/\D/g, '') || '0');

  return (
    <Tooltip>
      <TooltipTrigger asChild>
        <div
          ref={ref}
          onMouseDown={handleDown}
          onTouchStart={handleDown}
          className="gt-grab inline-flex transition-[opacity,filter] duration-300"
          style={{
            ['--rot' as string]: b.rot,
            animation: 'gt-idle-float 4s ease-in-out infinite',
            animationDelay: `${(idDigits % 7) * 0.2}s`,
            opacity: disabled ? 0.25 : 1,
            filter: disabled ? 'grayscale(0.6)' : 'none',
            transform: `rotate(${b.rot})`,
            touchAction: 'none',
          }}
        >
          <Bottle size={b.size} tint={b.tint} kind={b.kind} invalid={b.invalid} />
        </div>
      </TooltipTrigger>
      <TooltipContent side="bottom" className="bg-white text-ink border border-line shadow-2 px-3 py-2">
        <div className="flex flex-col gap-[2px] text-[11px]">
          <div className="font-semibold text-ink">
            {kindLabelOf(b)} · {sizeLabelOf(b.size)}
          </div>
          <div className="text-ink-3">
            <span className="text-ink-4">Cor: </span>{tintLabelOf(b.tint)}
          </div>
          <div className="text-ink-3">
            <span className="text-ink-4">Material: </span>{materialLabelOf(b)}
          </div>
        </div>
      </TooltipContent>
    </Tooltip>
  );
}
