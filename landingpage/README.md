# Zusammenspiel Landing Page

Eine moderne, responsive Landing Page für die Team-Organisations-App "Zusammenspiel".

## 🎯 Features

- **Responsive Design**: Optimiert für Desktop, Tablet und Mobile
- **Modern UI**: Sauberes, professionelles Design mit Inter Font
- **Performance**: Schnelle Ladezeiten und optimierte Assets
- **Accessibility**: Semantisches HTML und keyboard navigation
- **SEO-optimiert**: Meta tags und strukturierte Daten

## 📁 Struktur

```
landingpage/
├── index.html          # Haupt-HTML-Datei
├── styles.css          # CSS-Styles
├── script.js           # JavaScript-Funktionalität
└── README.md           # Diese Datei
```

## 🎨 Design-Features

### Hero Section

- Compelling value proposition
- Call-to-action buttons
- Interaktive Statistiken
- Mobile App Mockup

### Features Section

- 6 Kern-Features mit Icons
- Hover-Effekte
- Card-basiertes Layout

### How it Works

- 4-Schritt Prozess
- Visuell ansprechende Schritte
- Einfache Erklärungen

### Pricing

- 3 Preisstufen
- Featured Plan hervorgehoben
- Klare Feature-Listen

### Footer

- Links zu wichtigen Seiten
- Rechtliche Links
- Branding

## 🚀 Deployment

### Statisches Hosting

Die Landing Page kann auf jedem statischen Hosting-Service deployed werden:

- **Netlify**: Einfaches Drag & Drop
- **Vercel**: Git-Integration
- **GitHub Pages**: Direktes Repository-Hosting
- **Cloudflare Pages**: Schnelles globales CDN

### Domain Setup

Für die Produktivumgebung empfohlene Domain-Struktur:

- `zusammenspiel.app` → Landing Page
- `app.zusammenspiel.app` → Lamdera App

## 📊 Performance

- **Lighthouse Score**: 95+ angestrebt
- **First Contentful Paint**: < 1.5s
- **Largest Contentful Paint**: < 2.5s
- **Cumulative Layout Shift**: < 0.1

## 🔧 Anpassungen

### Farben

Die Hauptfarben können in `styles.css` angepasst werden:

```css
:root {
  --primary: #3b82f6;
  --primary-dark: #2563eb;
  --text: #1e293b;
  --text-light: #64748b;
}
```

### Content

- Statistiken in der Hero Section
- Feature-Beschreibungen
- Preise und Pläne
- Kontakt-Informationen

### Analytics

Für Production:

```javascript
// Google Analytics 4
gtag("config", "GA_MEASUREMENT_ID");

// Event Tracking
gtag("event", "click", {
  event_category: "CTA",
  event_label: button_text,
});
```

## 🎯 Conversion Optimierung

### A/B Testing Ideen

- Hero-Headline Varianten
- CTA-Button Farben und Texte
- Pricing-Positionierung
- Feature-Reihenfolge

### Lead Magnets

- Kostenlose Team-Templates
- Organisationsguide PDF
- Video-Tutorials

## 📱 Mobile Optimierung

- Touch-friendly Buttons (44px minimum)
- Optimierte Typografie
- Schnelle Ladezeiten
- App-like Erlebnis

## 🔒 Rechtliches

Die Landing Page benötigt noch:

- [ ] Datenschutzerklärung
- [ ] Impressum
- [ ] Cookie-Policy
- [ ] AGB

## 🚀 Next Steps

1. **Domain registrieren**: zusammenspiel.app
2. **SSL-Zertifikat** einrichten
3. **Analytics** implementieren
4. **Performance** optimieren
5. **SEO** implementieren
6. **A/B Tests** starten

## 📞 Support

Bei Fragen zur Landing Page oder technischen Anpassungen:

- Code-Review in GitHub
- Deployment-Support
- Performance-Optimierung
