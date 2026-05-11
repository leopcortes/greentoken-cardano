import { BrowserRouter, Routes, Route } from 'react-router-dom';
import { Toaster } from 'sonner';
import { DashboardPage } from '@/pages/Dashboard/DashboardPage';
import { CurrentBottlePage } from './pages/GreenStation/CurrentBottlePage';
import { CurrentWalletPage } from './pages/GreenStation/CurrentWalletPage';
import { CurrentContainerPage } from './pages/GreenStation/CurrentContainerPage';
import { InventoryPage } from './pages/GreenStation/InventoryPage';
import { PipelinePanel } from './pages/GreenStation/PipelinePanel';
import { StationProvider } from './pages/GreenStation/StationContext';
import { TopBar } from './pages/GreenStation/TopBar';

function HomePage() {
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
    </StationProvider>
  );
}

function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<HomePage />} />
        <Route path="/dashboard" element={<DashboardPage />} />
      </Routes>

      <Toaster position="bottom-center" richColors closeButton />
    </BrowserRouter>
  );
}

export default App;
