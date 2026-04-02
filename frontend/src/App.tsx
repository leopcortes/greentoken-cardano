import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Avatar, AvatarFallback, AvatarImage } from "@/components/ui/avatar"
import { BottlesPage } from '@/pages/BottlesPage';
import { UsersPage } from '@/pages/UsersPage';
import { ContainersPage } from '@/pages/ContainersPage';
import { RoutesPage } from '@/pages/RoutesPage';

function App() {
  return (
    <div className="min-h-screen bg-background">
      <header className="border-b bg-card px-6 py-4">
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
      </header>

      <main className="mx-auto 2xl:max-w-[75%] lg:max-w-[90%] max-w-[95%] p-6">
        <Tabs defaultValue="bottles">
          <TabsList>
            <TabsTrigger value="bottles">Garrafas</TabsTrigger>
            <TabsTrigger value="users">Usuários</TabsTrigger>
            <TabsTrigger value="containers">Containers</TabsTrigger>
            <TabsTrigger value="routes">Rotas</TabsTrigger>
          </TabsList>

          <TabsContent value="bottles" className="mt-4">
            <BottlesPage />
          </TabsContent>
          <TabsContent value="users" className="mt-4">
            <UsersPage />
          </TabsContent>
          <TabsContent value="containers" className="mt-4">
            <ContainersPage />
          </TabsContent>
          <TabsContent value="routes" className="mt-4">
            <RoutesPage />
          </TabsContent>
        </Tabs>
      </main>
    </div>
  );
}

export default App;
