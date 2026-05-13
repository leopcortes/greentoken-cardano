// QR code decorativo (SVG estatico). NAO codifica nada real - serve apenas
// como mock visual no login do reciclador. A leitura real de QR via app de
// carteira fica fora de escopo desta demo (TCC).
interface QrCodeMockProps {
  size?: number;
  className?: string;
  scanning?: boolean;
}

export function QrCodeMock({ size = 160, className, scanning = false }: QrCodeMockProps) {
  // Grade 21x21 (versao 1 do padrao QR) com 3 finder patterns nos cantos
  // + algumas celulas pseudo-aleatorias. Padrao fixo para nao parecer
  // animado entre renders.
  const cells: Array<[number, number]> = [
    // Linhas geradas para parecer um QR plausivel - nada decodificavel
    [4, 1], [6, 1], [8, 1], [12, 1], [13, 1], [16, 1],
    [4, 3], [10, 3], [11, 3], [13, 3], [15, 3],
    [5, 4], [8, 4], [9, 4], [13, 4],
    [4, 5], [6, 5], [12, 5], [15, 5],
    [8, 6], [11, 6], [14, 6],
    [1, 8], [3, 8], [4, 8], [6, 8], [8, 8], [10, 8], [12, 8], [15, 8], [17, 8], [18, 8],
    [2, 9], [4, 9], [7, 9], [11, 9], [13, 9], [16, 9],
    [0, 10], [5, 10], [9, 10], [10, 10], [14, 10], [17, 10],
    [3, 11], [6, 11], [8, 11], [12, 11], [15, 11], [18, 11],
    [1, 12], [4, 12], [10, 12], [13, 12], [16, 12],
    [9, 14], [11, 14], [14, 14], [17, 14],
    [8, 15], [10, 15], [13, 15], [16, 15], [19, 15],
    [9, 16], [12, 16], [15, 16], [18, 16],
    [11, 17], [14, 17], [17, 17], [19, 17],
    [10, 18], [13, 18], [16, 18],
    [9, 19], [12, 19], [15, 19], [18, 19],
  ];

  // Finder pattern 7x7 (3 cantos: top-left, top-right, bottom-left)
  const finderPositions: Array<[number, number]> = [
    [0, 0], [14, 0], [0, 14],
  ];

  return (
    <svg
      width={size}
      height={size}
      viewBox="0 0 21 21"
      className={className}
      role="img"
      aria-label="QR code de exemplo"
      style={{
        background: 'white',
        padding: '6px',
        borderRadius: '8px',
        boxShadow: scanning ? '0 0 0 2px rgba(34,197,94,0.6)' : undefined,
        transition: 'box-shadow 150ms ease',
      }}
    >
      {/* Finder patterns: borda externa 7x7, espaco branco, miolo 3x3 */}
      {finderPositions.map(([fx, fy], i) => (
        <g key={`finder-${i}`}>
          <rect x={fx} y={fy} width={7} height={7} fill="black" />
          <rect x={fx + 1} y={fy + 1} width={5} height={5} fill="white" />
          <rect x={fx + 2} y={fy + 2} width={3} height={3} fill="black" />
        </g>
      ))}
      {/* Celulas de dados (mock) */}
      {cells.map(([x, y], i) => (
        <rect key={`cell-${i}`} x={x} y={y} width={1} height={1} fill="black" />
      ))}
      {/* Linha de scan animada (SMIL) - so' aparece quando scanning=true */}
      {scanning && (
        <rect x={0} width={21} height={0.8} fill="rgba(34,197,94,0.85)">
          <animate
            attributeName="y"
            from="-1"
            to="21"
            dur="0.9s"
            repeatCount="indefinite"
          />
        </rect>
      )}
    </svg>
  );
}
