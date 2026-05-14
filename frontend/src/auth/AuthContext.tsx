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
  getAuthMe,
  getAuthToken,
  login as apiLogin,
  signup as apiSignup,
  setAuthToken,
  setUnauthorizedHandler,
  type SignupPayload,
  type SignupResponse,
  type User,
} from '@/services/api';

interface AuthState {
  // User completo do banco (id, role, name, email, wallet_address, ...).
  // null antes da reidratacao ou apos logout.
  user: User | null;
  ready: boolean;
  login: (email: string, password: string) => Promise<User>;
  signup: (payload: SignupPayload) => Promise<SignupResponse>;
  // Completa o login com token+user já obtidos (usado após MnemonicGate no signup).
  loginWithToken: (token: string, user: User) => void;
  logout: () => void;
}

const AuthCtx = createContext<AuthState | null>(null);

export function AuthProvider({ children }: { children: ReactNode }) {
  const [user, setUser] = useState<User | null>(null);
  const [ready, setReady] = useState(false);

  const logout = useCallback(() => {
    setAuthToken(null);
    setUser(null);
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
    });
    return () => setUnauthorizedHandler(null);
  }, []);

  const login = useCallback(async (email: string, password: string) => {
    const { token, user: u } = await apiLogin(email, password);
    setAuthToken(token);
    setUser(u);
    return u;
  }, []);

  const signup = useCallback(async (payload: SignupPayload) => {
    const resp = await apiSignup(payload);
    // Quando o backend retorna mnemonic, o usuário ainda não confirmou que anotou
    // as 24 palavras. Não fazemos login até ele confirmar (MnemonicGate).
    if (!resp.mnemonic) {
      setAuthToken(resp.token);
      setUser(resp.user);
    }
    return resp;
  }, []);

  const loginWithToken = useCallback((token: string, user: User) => {
    setAuthToken(token);
    setUser(user);
  }, []);

  const value = useMemo<AuthState>(
    () => ({ user, ready, login, signup, loginWithToken, logout }),
    [user, ready, login, signup, loginWithToken, logout],
  );

  return <AuthCtx.Provider value={value}>{children}</AuthCtx.Provider>;
}

export function useAuth(): AuthState {
  const ctx = useContext(AuthCtx);
  if (!ctx) throw new Error('useAuth must be used inside <AuthProvider>');
  return ctx;
}
