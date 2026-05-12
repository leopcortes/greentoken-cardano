import { BrowserRouter, Routes, Route } from 'react-router-dom';
import { Toaster } from 'sonner';
import { AuthProvider, useAuth } from '@/auth/AuthContext';
import { RequireRole } from '@/auth/RequireRole';
import { DashboardPage } from '@/pages/Dashboard/DashboardPage';
import { CurrentBottlePage } from './pages/GreenStation/CurrentBottlePage';
import { CurrentWalletPage } from './pages/GreenStation/CurrentWalletPage';
import { CurrentContainerPage } from './pages/GreenStation/CurrentContainerPage';
import { InventoryPage } from './pages/GreenStation/InventoryPage';
import { PipelinePanel } from './pages/GreenStation/PipelinePanel';
import { StationProvider } from './pages/GreenStation/StationContext';
import { TopBar } from './pages/GreenStation/TopBar';
import { KioskIdleScreen } from './pages/GreenStation/KioskIdleScreen';
import { OwnerLoginPage } from './pages/Login/OwnerLoginPage';
import { WalletPage } from './pages/Wallet/WalletPage';

function KioskRoute() {
  const { user, ready } = useAuth();
  if (!ready) {
    return (
      <div className="min-h-screen flex items-center justify-center text-ink-3 text-sm">
        Carregando...
      </div>
    );
  }
  // Kiosk so' libera a UI da station para recyclers logados. Owner logado vai
  // direto para o dashboard - se cair aqui, mostramos o idle screen para evitar
  // confusao (operador nao deve inserir garrafas com token de owner).
  if (!user || user.role !== 'recycler') {
    return <KioskIdleScreen />;
  }
  return (
    <StationProvider>
      <div className="min-h-screen lg:h-screen flex flex-col bg-bg lg:overflow-hidden">
        <header className="border-b border-line bg-bg-card px-6 py-4 flex-none">
          <TopBar />
        </header>

        <main className="flex-1 lg:min-h-0 px-4 py-4 grid grid-cols-1 lg:grid-cols-10 gap-4">
          <div className="flex flex-col gap-4 lg:col-span-3 lg:min-h-0">
            <CurrentBottlePage />
            <CurrentWalletPage />
          </div>

          <div className="lg:col-span-4 lg:min-h-0 min-h-[480px]">
            <CurrentContainerPage />
          </div>

          <div className="flex flex-col gap-4 lg:col-span-3 lg:min-h-0 min-h-[420px]">
            <div className="flex-1 min-h-0">
              <InventoryPage />
            </div>
            <PipelinePanel />
          </div>
        </main>
      </div>
      <Toaster position="bottom-center" richColors closeButton />
    </StationProvider>
  );
}

function App() {
  return (
    <AuthProvider>
      <BrowserRouter>
        <Routes>
          <Route path="/" element={<KioskRoute />} />
          <Route path="/login/owner" element={<OwnerLoginPage />} />
          <Route
            path="/wallet"
            element={
              <RequireRole role="recycler" redirectTo="/">
                <WalletPage />
              </RequireRole>
            }
          />
          <Route
            path="/dashboard"
            element={
              <RequireRole role="owner" redirectTo="/login/owner">
                <DashboardPage />
              </RequireRole>
            }
          />
        </Routes>
      </BrowserRouter>
    </AuthProvider>
  );
}

export default App;
