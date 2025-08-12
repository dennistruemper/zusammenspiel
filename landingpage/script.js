// Smooth scrolling for navigation links
document.addEventListener("DOMContentLoaded", function () {
  // Mobile menu toggle (if we add one later)
  const mobileMenuButton = document.querySelector(".mobile-menu-button");
  const navLinks = document.querySelector(".nav-links");

  // Smooth scroll for anchor links
  document.querySelectorAll('a[href^="#"]').forEach((anchor) => {
    anchor.addEventListener("click", function (e) {
      e.preventDefault();
      const target = document.querySelector(this.getAttribute("href"));
      if (target) {
        const headerHeight = document.querySelector(".header").offsetHeight;
        const targetPosition = target.offsetTop - headerHeight;

        window.scrollTo({
          top: targetPosition,
          behavior: "smooth",
        });
      }
    });
  });

  // Header background on scroll
  window.addEventListener("scroll", function () {
    const header = document.querySelector(".header");
    if (window.scrollY > 100) {
      header.style.background = "rgba(255, 255, 255, 0.98)";
    } else {
      header.style.background = "rgba(255, 255, 255, 0.95)";
    }
  });

  // Intersection Observer for fade-in animations
  const observerOptions = {
    threshold: 0.1,
    rootMargin: "0px 0px -50px 0px",
  };

  const observer = new IntersectionObserver(function (entries) {
    entries.forEach((entry) => {
      if (entry.isIntersecting) {
        entry.target.style.opacity = "1";
        entry.target.style.transform = "translateY(0)";
      }
    });
  }, observerOptions);

  // Observe elements for animation
  document
    .querySelectorAll(".feature-card, .step, .pricing-card")
    .forEach((el) => {
      el.style.opacity = "0";
      el.style.transform = "translateY(30px)";
      el.style.transition = "all 0.6s ease-out";
      observer.observe(el);
    });

  // Form validation (if we add contact forms later)
  function validateEmail(email) {
    const re = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    return re.test(email);
  }

  // Track CTA clicks (placeholder for analytics)
  document
    .querySelectorAll(".btn-primary, .btn-secondary")
    .forEach((button) => {
      button.addEventListener("click", function (e) {
        const buttonText = this.textContent.trim();
        console.log("CTA clicked:", buttonText);

        // Here you would integrate with your analytics service
        // gtag('event', 'click', { event_category: 'CTA', event_label: buttonText });
      });
    });

  // Easter egg: Konami code
  let konamiCode = "";
  const konami = "38384040373937396665";

  document.addEventListener("keydown", function (e) {
    konamiCode += e.keyCode;
    if (konamiCode.length > konami.length) {
      konamiCode = konamiCode.slice(1);
    }

    if (konamiCode === konami) {
      document.body.style.transform = "rotate(360deg)";
      document.body.style.transition = "transform 2s ease";
      setTimeout(() => {
        document.body.style.transform = "";
        alert(
          "🎉 Du hast den Konami Code gefunden! Zusammenspiel Entwickler grüßen dich!"
        );
      }, 2000);
    }
  });
});

// Service Worker registration (for future PWA features)
if ("serviceWorker" in navigator) {
  window.addEventListener("load", function () {
    // navigator.serviceWorker.register('/sw.js');
  });
}

// Performance monitoring
window.addEventListener("load", function () {
  if ("performance" in window) {
    const loadTime =
      performance.timing.loadEventEnd - performance.timing.navigationStart;
    console.log("Page load time:", loadTime + "ms");

    // Send to analytics if needed
    // gtag('event', 'timing_complete', { name: 'load', value: loadTime });
  }
});
