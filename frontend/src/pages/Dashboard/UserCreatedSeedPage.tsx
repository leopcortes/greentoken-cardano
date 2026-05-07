import { useEffect, useState } from 'react';
import { Dialog, DialogContent, DialogHeader, DialogTitle } from '@/components/ui/dialog';
import { Button } from '@/components/ui/button';
import { CopyButton } from '@/components/ui/copy-button';
import { truncateMiddle } from '@/lib/truncate';

interface SeedData {
  name: string;
  address: string;
  mnemonic: string[];
}

interface Props {
  data: SeedData | null;
  onClose: () => void;
}

export function UserCreatedSeedDialog({ data, onClose }: Props) {
  const [confirmed, setConfirmed] = useState(false);

  useEffect(() => {
    if (data) setConfirmed(false);
  }, [data]);

  return (
    <Dialog open={!!data} onOpenChange={(open) => { if (!open && confirmed) onClose(); }}>
      <DialogContent
        className="max-w-2xl"
        onPointerDownOutside={(e) => e.preventDefault()}
        onEscapeKeyDown={(e) => e.preventDefault()}
      >
        <DialogHeader>
          <DialogTitle>Frase de recuperação - {data?.name}</DialogTitle>
        </DialogHeader>

        <div className="space-y-4">
          <div className="rounded-md border border-amber-300 bg-amber-50 p-3 text-sm text-amber-900">
            <strong>Atenção:</strong> esta frase de 24 palavras é a única forma de
            recuperar a greenwallet deste usuário. Ela <strong>não será exibida
            novamente</strong>. Anote em local seguro antes de prosseguir.
          </div>

          {data && (
            <>
              <div className="space-y-1">
                <div className="text-xs text-muted-foreground">Endereço Cardano</div>
                <div className="flex items-center gap-1">
                  <span className="font-mono text-xs">{truncateMiddle(data.address, 40, 12)}</span>
                  <CopyButton value={data.address} />
                </div>
              </div>

              <div className="space-y-2">
                <div className="flex items-center justify-between">
                  <div className="text-xs text-muted-foreground">Frase de 24 palavras</div>
                  <CopyButton value={data.mnemonic.join(' ')} />
                </div>
                <div className="grid grid-cols-3 gap-2 rounded-md border bg-muted/30 p-3">
                  {data.mnemonic.map((word, i) => (
                    <div key={i} className="flex items-center gap-2 font-mono text-sm">
                      <span className="w-6 text-right text-xs text-muted-foreground">{i + 1}.</span>
                      <span>{word}</span>
                    </div>
                  ))}
                </div>
              </div>
            </>
          )}

          <label className="flex items-start gap-2 text-sm cursor-pointer">
            <input
              type="checkbox"
              checked={confirmed}
              onChange={(e) => setConfirmed(e.target.checked)}
              className="mt-1 h-4 w-4 cursor-pointer"
            />
            <span>
              Já anotei a frase de 24 palavras em um local seguro. Entendo que sem
              ela é impossível recuperar acesso à greenwallet.
            </span>
          </label>

          <Button
            type="button"
            className="w-full"
            disabled={!confirmed}
            onClick={onClose}
          >
            Concluir
          </Button>
        </div>
      </DialogContent>
    </Dialog>
  );
}
