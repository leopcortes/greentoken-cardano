import { useEffect, useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Dialog, DialogContent, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { CopyButton } from '@/components/ui/copy-button';
import { ErrorAlert } from '@/components/ui/error-alert';
import { SortableHeader } from '@/components/ui/sortable-header';
import { useSortable } from '@/hooks/useSortable';
import { getUsers, createUser, getUserRewards, type User, type Reward } from '@/services/api';
import { truncateMiddle } from '@/lib/truncate';

export function UsersPage() {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [dialogOpen, setDialogOpen] = useState(false);
  const [formRole, setFormRole] = useState<string>('recycler');
  const [formName, setFormName] = useState('');
  const [formEmail, setFormEmail] = useState('');
  const [formAddress, setFormAddress] = useState('');
  const [formPubkeyHash, setFormPubkeyHash] = useState('');
  const [submitting, setSubmitting] = useState(false);

  const [rewardsUser, setRewardsUser] = useState<User | null>(null);
  const [rewards, setRewards] = useState<Reward[]>([]);
  const [totalGreentoken, setTotalGreentoken] = useState(0);

  const { sorted, sortKey, sortDir, toggleSort } = useSortable<User>(users);

  const fetchUsers = async () => {
    try {
      setLoading(true);
      const data = await getUsers();
      setUsers(data);
      setError('');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar usuários');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => { fetchUsers(); }, []);

  const handleCreate = async (e: React.FormEvent) => {
    e.preventDefault();
    setSubmitting(true);
    try {
      await createUser({
        role: formRole,
        name: formName,
        email: formEmail,
        wallet_address: formAddress,
        pubkey_hash: formPubkeyHash,
      });
      setDialogOpen(false);
      setFormRole('recycler');
      setFormName('');
      setFormEmail('');
      setFormAddress('');
      setFormPubkeyHash('');
      fetchUsers();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao criar usuário');
    } finally {
      setSubmitting(false);
    }
  };

  const handleViewRewards = async (user: User) => {
    try {
      const data = await getUserRewards(user.id);
      setRewards(data.rewards);
      setTotalGreentoken(data.total_greentoken);
      setRewardsUser(user);
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Erro ao carregar recompensas');
    }
  };

  const SH = (label: string, key: keyof User) => (
    <SortableHeader label={label} sortKey={key as string} currentKey={sortKey as string | null} direction={sortDir} onSort={() => toggleSort(key)} />
  );

  return (
    <div className="space-y-3">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-semibold">Usuários</h2>
        <div className="flex gap-2">
          <Button variant="outline" size="sm" onClick={fetchUsers}>Atualizar</Button>
          <Dialog open={dialogOpen} onOpenChange={setDialogOpen}>
            <DialogTrigger asChild>
              <Button size="sm">+ Novo Usuário</Button>
            </DialogTrigger>
            <DialogContent>
              <DialogHeader>
                <DialogTitle>Criar Usuário</DialogTitle>
              </DialogHeader>
              <form onSubmit={handleCreate} className="space-y-4">
                <div className="space-y-2">
                  <Label htmlFor="user-role">Cargo</Label>
                  <select
                    id="user-role"
                    value={formRole}
                    onChange={e => setFormRole(e.target.value)}
                    className="flex h-10 w-full rounded-md border border-input bg-background px-3 py-2 text-sm"
                  >
                    <option value="recycler">Recycler</option>
                    <option value="owner">Owner</option>
                  </select>
                </div>
                <div className="space-y-2">
                  <Label htmlFor="user-name">Nome</Label>
                  <Input id="user-name" value={formName} onChange={e => setFormName(e.target.value)} required placeholder="Maria Silva" />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="user-email">Email</Label>
                  <Input id="user-email" type="email" value={formEmail} onChange={e => setFormEmail(e.target.value)} required placeholder="email@exemplo.com" />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="user-address">Endereço Cardano</Label>
                  <Input id="user-address" value={formAddress} onChange={e => setFormAddress(e.target.value)} required placeholder="addr_test1q..." />
                </div>
                <div className="space-y-2">
                  <Label htmlFor="user-pubkey">Pubkey Hash</Label>
                  <Input id="user-pubkey" value={formPubkeyHash} onChange={e => setFormPubkeyHash(e.target.value)} required placeholder="hex pubkey hash" />
                </div>
                <Button type="submit" disabled={submitting} className="w-full">
                  {submitting ? 'Criando...' : 'Criar'}
                </Button>
              </form>
            </DialogContent>
          </Dialog>
        </div>
      </div>

      {error && <ErrorAlert message={error} onDismiss={() => setError('')} />}

      <Card>
        <CardHeader className="py-3 px-4">
          <CardTitle className="text-sm font-medium text-muted-foreground">
            {loading ? 'Carregando...' : `${users.length} usuário(s) encontrado(s)`}
          </CardTitle>
        </CardHeader>
        <CardContent className="px-4 pb-4 pt-0">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>{SH('Nome', 'name')}</TableHead>
                <TableHead>{SH('Email', 'email')}</TableHead>
                <TableHead>{SH('Cargo', 'role')}</TableHead>
                <TableHead>Wallet</TableHead>
                <TableHead>{SH('Criado em', 'created_at')}</TableHead>
                <TableHead>Ações</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {sorted.map(user => (
                <TableRow key={user.id}>
                  <TableCell className="font-medium text-sm">{user.name}</TableCell>
                  <TableCell className="text-sm">{user.email}</TableCell>
                  <TableCell>
                    <Badge variant="secondary">{user.role}</Badge>
                  </TableCell>
                  <TableCell>
                    <div className="flex items-center gap-0.5 min-w-0">
                      <span className="font-mono text-xs" title={user.wallet_address}>
                        {truncateMiddle(user.wallet_address, 50, 10)}
                      </span>
                      <CopyButton className='ml-1' value={user.wallet_address} />
                    </div>
                  </TableCell>
                  <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                    {new Date(user.created_at).toLocaleDateString('pt-BR')}
                  </TableCell>
                  <TableCell>
                    <Button variant="outline" size="sm" className="h-7 text-xs px-2" onClick={() => handleViewRewards(user)}>
                      Recompensas
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
              {!loading && users.length === 0 && (
                <TableRow>
                  <TableCell colSpan={6} className="text-center text-muted-foreground py-8">
                    Nenhum usuário encontrado
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </CardContent>
      </Card>

      <Dialog open={!!rewardsUser} onOpenChange={() => setRewardsUser(null)}>
        <DialogContent className="max-w-3xl">
          <DialogHeader>
            <DialogTitle>Recompensas - {rewardsUser?.name}</DialogTitle>
          </DialogHeader>
          <div className="space-y-3">
            <div className="flex items-center gap-2 text-lg font-semibold">
              <span className="text-green-600">Total: {totalGreentoken} Greentoken</span>
            </div>
            {rewards.length > 0 ? (
              <Table>
                <TableHeader>
                  <TableRow>
                    <TableHead>Garrafa</TableHead>
                    <TableHead>Estágio</TableHead>
                    <TableHead>Greentoken</TableHead>
                    <TableHead>Data</TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {rewards.map(r => (
                    <TableRow key={r.id}>
                      <TableCell>
                        <div className="flex items-center gap-0.5">
                          <span className="font-mono text-xs">{r.bottle_id}...</span>
                          <CopyButton className='ml-1' value={r.bottle_id} />
                        </div>
                      </TableCell>
                      <TableCell className="text-sm">{r.stage}</TableCell>
                      <TableCell className="font-semibold text-green-600">+{r.greentoken_amount}</TableCell>
                      <TableCell className="text-xs text-muted-foreground">
                        {new Date(r.sent_at).toLocaleDateString('pt-BR')}
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            ) : (
              <p className="text-sm text-muted-foreground">Nenhuma recompensa registrada</p>
            )}
          </div>
        </DialogContent>
      </Dialog>
    </div>
  );
}
