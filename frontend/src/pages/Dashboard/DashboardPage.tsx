import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Avatar, AvatarFallback, AvatarImage } from "@/components/ui/avatar"
import { Toaster } from 'sonner';
import { BottlesPage } from '@/pages/Dashboard/BottlesPage';
import { UsersPage } from '@/pages/Dashboard/UsersPage';
import { ContainersPage } from '@/pages/Dashboard/ContainersPage';
import { RoutesPage } from '@/pages/Dashboard/RoutesPage';
import { StationsPage } from '@/pages/Dashboard/StationsPage';
import { GreenwalletsPage } from '@/pages/Dashboard/GreenwalletsPage';
import { useNavigate } from 'react-router-dom';
import { LogOut } from 'lucide-react';
import { useAuth } from '@/auth/AuthContext';

export function DashboardPage() {
  const { logout } = useAuth();
  const navigate = useNavigate();
  const onLogout = () => {
    logout();
    navigate('/login/owner', { replace: true });
  };

  return (
    <div className="min-h-screen bg-background">
      <header className="border-b bg-card px-6 py-4 flex justify-between gap-4">
        <div className="flex items-center gap-3">
          <Avatar className="w-9 h-9 rounded-[10px]">
            <AvatarImage src="/unb_logo.png" className="object-cover" />
            <AvatarFallback className='bg-green-600 text-white font-bold text-sm'>₲</AvatarFallback>
          </Avatar>
          <div>
            <h1 className="text-md font-bold tracking-[-0.01em] leading-snug">Greentoken Dashboard</h1>
            <p className="text-[12px] text-ink-3 leading-tight">
              Rastreamento de reciclagem na blockchain Cardano
            </p>
          </div>
        </div>

        <div className="flex items-center gap-4">
          <div>
            <h1 className="text-base font-normal tracking-tight text-right leading-tight">Projeto de Conclusão de Curso de Eng. de Computação</h1>
            <p className="text-xs text-ink-3 text-right">Ana Paula Nóbrega e Leonardo Côrtes</p>
          </div>
          <button
            type="button"
            onClick={onLogout}
            className="inline-flex items-center gap-1.5 px-3 py-1.5 rounded-md border border-line text-ink-2 text-[12px] font-semibold hover:bg-bg-elev transition-colors"
          >
            <LogOut size={13} />
            Sair
          </button>
        </div>
      </header>

      <main className="mx-auto 2xl:max-w-[75%] lg:max-w-[95%] p-6">
        <Tabs defaultValue="bottles">
          <TabsList className='bg-gray-100 text-gray-800'>
            <TabsTrigger className='hover:bg-gray-50/60' value="users">Usuários</TabsTrigger>
            <TabsTrigger className='hover:bg-gray-50/60' value="bottles">Garrafas</TabsTrigger>
            <TabsTrigger className='hover:bg-gray-50/60' value="containers">Containers</TabsTrigger>
            <TabsTrigger className='hover:bg-gray-50/60' value="routes">Rotas</TabsTrigger>
            <TabsTrigger className='hover:bg-gray-50/60' value="stations">Estações</TabsTrigger>
            <TabsTrigger className='hover:bg-gray-50/60' value="wallets">Carteiras</TabsTrigger>
          </TabsList>

          <TabsContent value="users" className="mt-4">
            <UsersPage />
          </TabsContent>
          <TabsContent value="bottles" className="mt-4">
            <BottlesPage />
          </TabsContent>
          <TabsContent value="containers" className="mt-4">
            <ContainersPage />
          </TabsContent>
          <TabsContent value="routes" className="mt-4">
            <RoutesPage />
          </TabsContent>
          <TabsContent value="stations" className="mt-4">
            <StationsPage />
          </TabsContent>
          <TabsContent value="wallets" className="mt-4">
            <GreenwalletsPage />
          </TabsContent>
        </Tabs>
      </main>
      <Toaster position="bottom-right" richColors closeButton />
    </div>
  );
}
