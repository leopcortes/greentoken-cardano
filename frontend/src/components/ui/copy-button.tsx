import { useState } from 'react';
import { Check, Copy } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from '@/components/ui/tooltip';

interface CopyButtonProps {
  value: string;
  direction?: 'top' | 'right' | 'bottom' | 'left';
  className?: string;
}

export function CopyButton({ value, direction = 'right', className = '' }: CopyButtonProps) {
  const [copied, setCopied] = useState(false);

  const handleCopy = async (e: React.MouseEvent) => {
    e.stopPropagation();
    await navigator.clipboard.writeText(value);
    setCopied(true);
    setTimeout(() => setCopied(false), 1500);
  };

  return (
    <TooltipProvider>
      <Tooltip>
        <TooltipTrigger asChild>
          <Button
            size="icon"
            onClick={handleCopy}
            className={`inline-flex items-center justify-center h-5 w-5 rounded bg-transparent hover:bg-muted text-muted-foreground hover:text-foreground transition-colors ${className}`}
          >
            {copied ? (
              <Check className="h-3 w-3 text-green-600" />
            ) : (
              <Copy className="h-3 w-3" />
            )}
          </Button>
        </TooltipTrigger>

        <TooltipContent className="bg-white text-black border border-gray-100" side={direction}>
          <p>{copied ? 'Copiado!' : 'Copiar'}</p>
        </TooltipContent>
      </Tooltip>
    </TooltipProvider>
  );
}