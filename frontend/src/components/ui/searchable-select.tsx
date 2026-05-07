import { useState, useRef, useEffect } from 'react';
import { ChevronDown, Search, Check } from 'lucide-react';
import { cn } from '@/lib/utils';

export interface SearchableSelectOption {
  value: string;
  label: string;
  detail?: string;
}

interface SearchableSelectProps {
  options: SearchableSelectOption[];
  value: string;
  onValueChange: (value: string) => void;
  placeholder?: string;
  searchPlaceholder?: string;
  className?: string;
  error?: boolean;
}

export function SearchableSelect({
  options,
  value,
  onValueChange,
  placeholder = 'Selecione...',
  searchPlaceholder = 'Buscar...',
  className,
  error,
}: SearchableSelectProps) {
  const [open, setOpen] = useState(false);
  const [search, setSearch] = useState('');
  const [openUp, setOpenUp] = useState(false);
  const containerRef = useRef<HTMLDivElement>(null);
  const triggerRef = useRef<HTMLButtonElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);

  const selected = options.find(o => o.value === value);

  const filtered = options.filter(o => {
    const q = search.toLowerCase();
    return o.label.toLowerCase().includes(q) || (o.detail?.toLowerCase().includes(q));
  });

  useEffect(() => {
    if (!open) return;
    if (triggerRef.current) {
      const rect = triggerRef.current.getBoundingClientRect();
      const estimatedHeight = 320;
      const spaceBelow = window.innerHeight - rect.bottom;
      setOpenUp(spaceBelow < estimatedHeight && rect.top > spaceBelow);
    }
    inputRef.current?.focus();
  }, [open]);

  useEffect(() => {
    const handler = (e: MouseEvent) => {
      if (containerRef.current && !containerRef.current.contains(e.target as Node)) {
        setOpen(false);
        setSearch('');
      }
    };
    document.addEventListener('mousedown', handler);
    return () => document.removeEventListener('mousedown', handler);
  }, []);

  return (
    <div ref={containerRef} className={cn('relative', className)}>
      <button
        ref={triggerRef}
        type="button"
        onClick={() => setOpen(!open)}
        className={cn(
          'flex h-10 w-full items-center justify-between rounded-md border border-input bg-background px-3 py-2 text-sm ring-offset-background focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2',
          !selected && 'text-muted-foreground',
          error && 'border-red-500 focus:ring-red-500',
        )}
      >
        <span className="truncate">
          {selected ? selected.label : placeholder}
        </span>
        <ChevronDown className="h-4 w-4 opacity-50 shrink-0" />
      </button>

      {open && (
        <div
          className={cn(
            'absolute z-50 w-full rounded-md border bg-popover text-popover-foreground shadow-md animate-in fade-in-0 zoom-in-95',
            openUp ? 'bottom-full mb-1' : 'top-full mt-1',
          )}
        >
          <div className="flex items-center border-b px-3 py-2">
            <Search className="h-4 w-4 text-muted-foreground mr-2 shrink-0" />
            <input
              ref={inputRef}
              value={search}
              onChange={e => setSearch(e.target.value)}
              placeholder={searchPlaceholder}
              className="flex-1 bg-transparent text-sm outline-none placeholder:text-muted-foreground"
            />
          </div>
          <div className="max-h-48 overflow-y-auto p-1">
            {filtered.length === 0 ? (
              <div className="px-3 py-4 text-sm text-center text-muted-foreground">
                Nenhum resultado encontrado
              </div>
            ) : (
              filtered.map(option => (
                <button
                  key={option.value}
                  type="button"
                  onClick={() => {
                    onValueChange(option.value);
                    setOpen(false);
                    setSearch('');
                  }}
                  className={cn(
                    'flex w-full items-center gap-2 rounded-sm px-3 py-2 text-sm cursor-pointer hover:bg-accent hover:text-accent-foreground',
                    value === option.value && 'bg-accent',
                  )}
                >
                  <Check className={cn('h-4 w-4 shrink-0', value === option.value ? 'opacity-100' : 'opacity-0')} />
                  <div className="flex-1 text-left min-w-0">
                    <div className="truncate">{option.label}</div>
                    {option.detail && (
                      <div className="text-xs text-muted-foreground truncate">{option.detail}</div>
                    )}
                  </div>
                </button>
              ))
            )}
          </div>
        </div>
      )}
    </div>
  );
}
