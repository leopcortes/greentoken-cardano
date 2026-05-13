import { useCallback, useEffect, useRef } from 'react';

// Dispara `onIdle` apos `idleMs` sem chamadas a `reset`. Util para deslogar
// o reciclador do terminal apos um periodo de inatividade.
//
// O hook NAO escuta eventos do DOM por si so - o caller deve chamar reset()
// nos pontos relevantes (ex.: drop de garrafa, troca de container) para que
// "atividade" seja explicita e não ruido visual (movimento de mouse passa a
// não contar como atividade).
export function useIdleTimeout(idleMs: number, onIdle: () => void, enabled: boolean = true) {
  const onIdleRef = useRef(onIdle);
  onIdleRef.current = onIdle;

  const timerRef = useRef<ReturnType<typeof setTimeout> | null>(null);

  const clear = useCallback(() => {
    if (timerRef.current) {
      clearTimeout(timerRef.current);
      timerRef.current = null;
    }
  }, []);

  const reset = useCallback(() => {
    clear();
    if (!enabled) return;
    timerRef.current = setTimeout(() => {
      onIdleRef.current();
    }, idleMs);
  }, [idleMs, enabled, clear]);

  useEffect(() => {
    if (!enabled) {
      clear();
      return;
    }
    reset();
    return clear;
  }, [enabled, reset, clear]);

  return { reset, clear };
}
