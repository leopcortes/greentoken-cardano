/** @type {import('tailwindcss').Config} */
export default {
  darkMode: 'class',
  content: [
    './index.html',
    './src/**/*.{js,ts,jsx,tsx,html}',
  ],
  theme: {
    extend: {
      borderRadius: {
        lg: 'var(--radius)',
        md: 'calc(var(--radius) - 2px)',
        sm: 'calc(var(--radius) - 4px)',
        'gt-sm': 'var(--r-sm)',
        'gt-md': 'var(--r-md)',
        'gt-lg': 'var(--r-lg)',
        'gt-xl': 'var(--r-xl)',
      },
      colors: {
        background: 'hsl(var(--background))',
        foreground: 'hsl(var(--foreground))',
        card: {
          DEFAULT: 'hsl(var(--card))',
          foreground: 'hsl(var(--card-foreground))',
        },
        popover: {
          DEFAULT: 'hsl(var(--popover))',
          foreground: 'hsl(var(--popover-foreground))',
        },
        primary: {
          DEFAULT: 'hsl(var(--primary))',
          foreground: 'hsl(var(--primary-foreground))',
        },
        secondary: {
          DEFAULT: 'hsl(var(--secondary))',
          foreground: 'hsl(var(--secondary-foreground))',
        },
        muted: {
          DEFAULT: 'hsl(var(--muted))',
          foreground: 'hsl(var(--muted-foreground))',
        },
        accent: {
          DEFAULT: 'hsl(var(--accent))',
          foreground: 'hsl(var(--accent-foreground))',
        },
        destructive: {
          DEFAULT: 'hsl(var(--destructive))',
          foreground: 'hsl(var(--destructive-foreground))',
        },
        border: 'hsl(var(--border))',
        input: 'hsl(var(--input))',
        ring: 'hsl(var(--ring))',
        bg: {
          DEFAULT: 'var(--bg)',
          elev: 'var(--bg-elev)',
          card: 'var(--bg-card)',
        },
        ink: {
          DEFAULT: 'var(--ink)',
          2: 'var(--ink-2)',
          3: 'var(--ink-3)',
          4: 'var(--ink-4)',
        },
        line: {
          DEFAULT: 'var(--line)',
          2: 'var(--line-2)',
        },
        gt: {
          50: 'var(--gt-50)',
          100: 'var(--gt-100)',
          200: 'var(--gt-200)',
          300: 'var(--gt-300)',
          400: 'var(--gt-400)',
          500: 'var(--gt-500)',
          600: 'var(--gt-600)',
          700: 'var(--gt-700)',
          800: 'var(--gt-800)',
          900: 'var(--gt-900)',
        },
        cdn: {
          DEFAULT: 'var(--cdn)',
          soft: 'var(--cdn-soft)',
        },
        warn: {
          DEFAULT: 'var(--warn)',
          soft: 'var(--warn-soft)',
        },
        err: {
          DEFAULT: 'var(--err)',
          soft: 'var(--err-soft)',
        },
      },
      fontFamily: {
        sans: ['var(--font-sans)', 'sans-serif'],
        mono: ['var(--font-mono)', 'monospace'],
      },
      boxShadow: {
        '1': 'var(--sh-1)',
        '2': 'var(--sh-2)',
        '3': 'var(--sh-3)',
        'pop': 'var(--sh-pop)',
      },
      keyframes: {
        'gt-ping': {
          '0%': { transform: 'scale(0.6)', opacity: '0.7' },
          '100%': { transform: 'scale(1.6)', opacity: '0' },
        },
        'gt-drop-glow': {
          '0%, 100%': { boxShadow: '0 0 0 0 rgba(34, 197, 94, 0)' },
          '50%': { boxShadow: '0 0 0 14px rgba(34, 197, 94, 0.10)' },
        },
        'gt-squash': {
          '0%': { transform: 'translateY(0) scaleY(1) scaleX(1)', opacity: '1' },
          '35%': { transform: 'translateY(8px) scaleY(0.6) scaleX(1.15)', opacity: '1' },
          '60%': { transform: 'translateY(14px) scaleY(0.25) scaleX(1.3)', opacity: '0.9' },
          '100%': { transform: 'translateY(20px) scaleY(0.05) scaleX(1.4)', opacity: '0' },
        },
        'gt-coin-fly': {
          '0%': { transform: 'translate(0,0) scale(0.7)', opacity: '0' },
          '10%': { opacity: '1' },
          '100%': { transform: 'translate(var(--tx, -300px), var(--ty, -200px)) scale(1.05)', opacity: '0' },
        },
        'gt-scan': {
          '0%': { transform: 'translateY(-2%)', opacity: '0' },
          '10%, 90%': { opacity: '1' },
          '100%': { transform: 'translateY(102%)', opacity: '0' },
        },
        'gt-bump': {
          '0%, 100%': { transform: 'scale(1)' },
          '35%': { transform: 'scale(1.12)', color: 'var(--gt-600)' },
        },
        'gt-node-pop': {
          '0%, 100%': { transform: 'scale(1)' },
          '0%': { transform: 'scale(0.8)' },
          '60%': { transform: 'scale(1.15)' },
        },
        'gt-idle-float': {
          '0%, 100%': { transform: 'translateY(0) rotate(var(--rot, 0deg))' },
          '50%': { transform: 'translateY(-3px) rotate(var(--rot, 0deg))' },
        },
        'gt-shake': {
          '0%, 100%': { transform: 'translateX(0)' },
          '20%, 60%': { transform: 'translateX(-6px)' },
          '40%, 80%': { transform: 'translateX(6px)' },
        },
      },
      animation: {
        'gt-ping': 'gt-ping 1.6s ease-out infinite',
        'gt-drop-glow': 'gt-drop-glow 2s ease-in-out infinite',
        'gt-squash': 'gt-squash 0.3s ease-out forwards',
        'gt-coin-fly': 'gt-coin-fly 0.6s ease-out forwards',
        'gt-scan': 'gt-scan 2s linear infinite',
        'gt-bump': 'gt-bump 0.5s ease-out',
        'gt-node-pop': 'gt-node-pop 0.3s ease-out',
        'gt-idle-float': 'gt-idle-float 3s ease-in-out infinite',
        'gt-shake': 'gt-shake 0.4s ease-in-out',
      },
    },
  },
  plugins: [require('tailwindcss-animate')],
}
