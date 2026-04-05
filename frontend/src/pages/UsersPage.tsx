import { useEffect, useState } from 'react';
import { toast } from 'sonner';
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
import { Select, SelectContent, SelectGroup, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select"
import { useSortable } from '@/hooks/useSortable';
import { getUsers, createUser, getUserRewards, type User, type Reward } from '@/services/api';
import { truncateMiddle } from '@/lib/truncate';
import { ROLE_LABELS, STAGE_LABELS, t } from '@/lib/labels';

const ROLE_COLORS: Record<string, string> = {
  owner: 'bg-blue-100 text-blue-800',
  recycler: 'bg-green-100 text-green-800',
};

export function UsersPage() {
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  const [dialogOpen, setDialogOpen] = useState(false);
  const [formRole, setFormRole] = useState<string>('recycler');
  const [formName, setFormName] = useState('');
  const [nameError, setNameError] = useState('');
  const [formEmail, setFormEmail] = useState('');
  const [emailError, setEmailError] = useState('');
  const [formAddress, setFormAddress] = useState('');
  const [addressError, setAddressError] = useState('');
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

    let hasError = false;

    if (!formName.trim()) {
      setNameError('O nome é obrigatório.');
      hasError = true;
    } else {
      setNameError('');
    }

    if (!formEmail.trim()) {
      setEmailError('O email é obrigatório.');
      hasError = true;
    } else {
      setEmailError('');
    }

    if (!formAddress.trim()) {
      setAddressError('O endereço Cardano é obrigatório.');
      hasError = true;
    } else {
      setAddressError('');
    }

    if (hasError) return;

    setSubmitting(true);
    try {
      const user = await createUser({
        role: formRole,
        name: formName,
        email: formEmail,
        wallet_address: formAddress,
      });

      toast.success(`Usuário "${user.name}" criado com sucesso.`, { duration: 5000 });
      setDialogOpen(false);
      setFormRole('recycler');
      setFormName('');
      setFormEmail('');
      setFormAddress('');
      setNameError('');
      setEmailError('');
      setAddressError('');
      fetchUsers();
    } catch (err) {
      toast.error(err instanceof Error ? err.message : 'Erro ao criar usuário', { duration: 10000 });
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
      toast.error(err instanceof Error ? err.message : 'Erro ao carregar recompensas', { duration: 10000 });
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
          <Button variant="outline" size="sm" className='bg-gray-100 text-gray-800' onClick={fetchUsers}>Atualizar</Button>
          <Dialog open={dialogOpen} onOpenChange={setDialogOpen}>
            <DialogTrigger asChild>
              <Button size="sm">+ Novo Usuário</Button>
            </DialogTrigger>
            <DialogContent className='max-w-3xl'>
              <DialogHeader>
                <DialogTitle>Criar Usuário</DialogTitle>
              </DialogHeader>
              <form onSubmit={handleCreate} className="space-y-4">
                <div className='space-y-2'>
                  <Label htmlFor="user-role">
                    Cargo<span className="text-red-500">*</span>
                  </Label>
                  <Select value={formRole} onValueChange={setFormRole}>
                    <SelectTrigger id="user-role" className="w-full">
                      <SelectValue placeholder="Cargo" />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectGroup>
                        <SelectItem value="recycler">Reciclador</SelectItem>
                        <SelectItem value="owner">Proprietário</SelectItem>
                      </SelectGroup>
                    </SelectContent>
                  </Select>
                </div>
                <div className="space-y-2">
                  <Label htmlFor="user-name">
                    Nome<span className="text-red-500">*</span>
                  </Label>
                  <Input
                    id="user-name"
                    value={formName}
                    onChange={e => {
                      setFormName(e.target.value);
                      if (nameError) setNameError('');
                    }}
                    placeholder="Maria Silva"
                    className={nameError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                  />
                  {nameError && (
                    <p className="text-sm font-medium text-red-500">{nameError}</p>
                  )}
                </div>

                <div className="space-y-2">
                  <Label htmlFor="user-email">
                    Email<span className="text-red-500">*</span>
                  </Label>
                  <Input
                    id="user-email"
                    type="email"
                    value={formEmail}
                    onChange={e => {
                      setFormEmail(e.target.value);
                      if (emailError) setEmailError('');
                    }}
                    placeholder="email@exemplo.com"
                    className={emailError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                  />
                  {emailError && (
                    <p className="text-sm font-medium text-red-500">{emailError}</p>
                  )}
                </div>

                <div className="space-y-2">
                  <Label htmlFor="user-address">
                    Endereço Cardano<span className="text-red-500">*</span>
                  </Label>
                  <Input
                    id="user-address"
                    value={formAddress}
                    onChange={e => {
                      setFormAddress(e.target.value);
                      if (addressError) setAddressError('');
                    }}
                    placeholder="addr_test1abcdefghijklmnopqrstuvwxyz..."
                    className={addressError ? 'border-red-500 focus-visible:ring-red-500' : ''}
                  />
                  {addressError && (
                    <p className="text-sm font-medium text-red-500">{addressError}</p>
                  )}
                </div>
                <p className="text-xs text-muted-foreground">
                  O pubkey hash será calculado automaticamente a partir do endereço.
                </p>
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
        <CardContent className="px-4 pb-2 pt-0">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>{SH('Usuário', 'name')}</TableHead>
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
                  <TableCell className="font-medium text-sm">
                    <div className="text-sm font-medium leading-tight">{user.name}</div>
                    <div className="flex items-center gap-0.5 mt-0.5">
                        <span className="font-mono text-[11px] text-muted-foreground hidden 2xl:block">{truncateMiddle(user.id, 30, 8)}</span>
                        <span className="font-mono text-[11px] text-muted-foreground block 2xl:hidden">{truncateMiddle(user.id, 10, 5)}</span>
                      <CopyButton className='ml-1' value={user.id} />
                    </div>
                  </TableCell>
                  <TableCell className="text-sm">{user.email}</TableCell>
                  <TableCell>
                    <Badge variant="secondary" className={ROLE_COLORS[user.role] || ''}>
                      {t(ROLE_LABELS, user.role)}
                    </Badge>
                  </TableCell>
                  <TableCell>
                    <div className="flex items-center gap-0.5 min-w-0">
                        <span className="font-mono text-xs">{truncateMiddle(user.wallet_address, 30, 10)}</span>
                      <CopyButton className='ml-1' value={user.wallet_address} />
                    </div>
                  </TableCell>
                  <TableCell className="text-xs text-muted-foreground whitespace-nowrap">
                    {new Date(user.created_at).toLocaleDateString('pt-BR')}
                  </TableCell>
                  <TableCell>
                    <Button variant="outline" size="sm" className="h-7 text-xs px-2 bg-gray-100" onClick={() => handleViewRewards(user)}>
                      Recompensas
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
              {!loading && users.length === 0 && (
                <TableRow>
                  <TableCell colSpan={7} className="text-center text-muted-foreground py-8">
                    Nenhum usuário encontrado
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </CardContent>
      </Card>

      <Dialog open={!!rewardsUser} onOpenChange={() => setRewardsUser(null)}>
        <DialogContent className="max-w-3xl" onOpenAutoFocus={(e) => e.preventDefault()}>
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
                      <TableCell className='flex gap-2'>
                        <div className="text-sm font-medium leading-tight">{r.bottle_name}</div>
                        <div className="flex items-center gap-0.5">
                          <span className="font-mono text-muted-foreground text-xs">({truncateMiddle(r.bottle_id, 10, 4)})</span>
                          <CopyButton value={r.bottle_id} />
                        </div>
                      </TableCell>
                      <TableCell className="text-sm">{t(STAGE_LABELS, r.stage)}</TableCell>
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
