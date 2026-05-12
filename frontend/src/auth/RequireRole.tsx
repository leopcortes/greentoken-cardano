import { type ReactNode } from 'react';
import { Navigate } from 'react-router-dom';
import { useAuth } from './AuthContext';

interface Props {
  role: 'owner' | 'recycler';
  // Para onde redirecionar quando o usuario nao esta autenticado ou tem outro
  // papel. Owner -> /login/owner (form de senha); recycler -> /  (kiosk idle).
  redirectTo: string;
  children: ReactNode;
}

export function RequireRole({ role, redirectTo, children }: Props) {
  const { user, ready } = useAuth();

  if (!ready) {
    return (
      <div className="min-h-screen flex items-center justify-center text-ink-3 text-sm">
        Carregando...
      </div>
    );
  }
  if (!user || user.role !== role) {
    return <Navigate to={redirectTo} replace />;
  }
  return <>{children}</>;
}
