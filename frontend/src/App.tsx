import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Avatar, AvatarFallback, AvatarImage } from "@/components/ui/avatar"
import { Toaster } from 'sonner';
import { BottlesPage } from '@/pages/BottlesPage';
import { UsersPage } from '@/pages/UsersPage';
import { ContainersPage } from '@/pages/ContainersPage';
import { RoutesPage } from '@/pages/RoutesPage';
import { StationsPage } from '@/pages/StationsPage';

function App() {
  return (
    <div className="min-h-screen bg-background">
      <header className="border-b bg-card px-6 py-4 flex justify-between">
        <div className="flex items-center gap-3">
          <Avatar className="h-8 w-8">
            <AvatarImage src="/unb_logo.png" className="object-cover" />
            <AvatarFallback className='bg-green-600 text-white font-bold text-sm'>G</AvatarFallback>
          </Avatar>
          <div>
            <h1 className="text-xl font-bold tracking-tight">Greentoken Dashboard</h1>
            <p className="text-sm text-muted-foreground">
              Rastreamento de reciclagem na blockchain Cardano
            </p>
          </div>
        </div>
        <div>
          <h1 className="text-base font-normal tracking-tight text-right">Projeto Final de Engenharia de Computação</h1>
          <p className="text-xs text-muted-foreground text-right">Ana Paula Oliveira da Nóbrega Costa</p>
          <p className="text-xs text-muted-foreground text-right">Leonardo Pereira Côrtes</p>
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
        </Tabs>
      </main>
      <Toaster position="bottom-right" richColors closeButton />
    </div>
  );
}

export default App;
