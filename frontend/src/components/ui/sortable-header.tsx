import { ArrowUp, ArrowDown, ArrowUpDown } from 'lucide-react';
import type { SortDirection } from '@/hooks/useSortable';

interface SortableHeaderProps {
  label: string;
  sortKey: string;
  currentKey: string | null;
  direction: SortDirection;
  onSort: () => void;
}

export function SortableHeader({ label, sortKey, currentKey, direction, onSort }: SortableHeaderProps) {
  const isActive = currentKey === sortKey;
  return (
    <button
      type="button"
      onClick={onSort}
      className="inline-flex items-center gap-1 hover:text-foreground transition-colors -ml-1 px-1 py-0.5 rounded"
    >
      {label}
      {isActive ? (
        direction === 'asc' ? <ArrowUp className="h-3 w-3" /> : <ArrowDown className="h-3 w-3" />
      ) : (
        <ArrowUpDown className="h-3 w-3 opacity-40" />
      )}
    </button>
  );
}
