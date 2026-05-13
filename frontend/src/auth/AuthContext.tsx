import {
  createContext,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useState,
  type ReactNode,
} from 'react';
import {
  AuthMePayload,
  getAuthMe,
  getAuthToken,
  loginOwner as apiLoginOwner,
  loginRecycler as apiLoginRecycler,
  setAuthToken,
  setUnauthorizedHandler,
  type User,
} from '@/services/api';

interface AuthState {
  user: AuthMePayload | null;
  // Dados completos do reciclador (quando role=recycler). null para owner ou
  // antes da reidratacao.
  recyclerProfile: User | null;
  ready: boolean;
  loginOwner: (password: string) => Promise<void>;
  loginRecycler: (walletAddress: string) => Promise<User>;
  logout: () => void;
}

const AuthCtx = createContext<AuthState | null>(null);

export function AuthProvider({ children }: { children: ReactNode }) {
  const [user, setUser] = useState<AuthMePayload | null>(null);
  const [recyclerProfile, setRecyclerProfile] = useState<User | null>(null);
  const [ready, setReady] = useState(false);

  const logout = useCallback(() => {
    setAuthToken(null);
    setUser(null);
    setRecyclerProfile(null);
  }, []);

  // Reidrata sessao a partir do token salvo em localStorage. Se /auth/me falhar
  // (token expirado), limpa a sessao silenciosamente.
  useEffect(() => {
    const token = getAuthToken();
    if (!token) {
      setReady(true);
      return;
    }
    let alive = true;
    getAuthMe()
      .then(({ user: u }) => {
        if (!alive) return;
        setUser(u);
      })
      .catch(() => {
        if (!alive) return;
        setAuthToken(null);
      })
      .finally(() => {
        if (alive) setReady(true);
      });
    return () => { alive = false; };
  }, []);

  // Quando o backend retorna 401, limpa a sessao para acionar o redirect dos
  // guards. Evita loops infinitos quando /auth/me esta sendo chamado.
  useEffect(() => {
    setUnauthorizedHandler(() => {
      setAuthToken(null);
      setUser(null);
      setRecyclerProfile(null);
    });
    return () => setUnauthorizedHandler(null);
  }, []);

  const loginOwner = useCallback(async (password: string) => {
    const { token } = await apiLoginOwner(password);
    setAuthToken(token);
    const { user: u } = await getAuthMe();
    setUser(u);
  }, []);

  const loginRecycler = useCallback(async (walletAddress: string) => {
    const { token, user: u } = await apiLoginRecycler(walletAddress);
    setAuthToken(token);
    setRecyclerProfile(u);
    const { user: payload } = await getAuthMe();
    setUser(payload);
    return u;
  }, []);

  const value = useMemo<AuthState>(
    () => ({ user, recyclerProfile, ready, loginOwner, loginRecycler, logout }),
    [user, recyclerProfile, ready, loginOwner, loginRecycler, logout],
  );

  return <AuthCtx.Provider value={value}>{children}</AuthCtx.Provider>;
}

export function useAuth(): AuthState {
  const ctx = useContext(AuthCtx);
  if (!ctx) throw new Error('useAuth must be used inside <AuthProvider>');
  return ctx;
}
