// Dashboard JavaScript

// Current Time and Date
function updateDateTime() {
    const now = new Date();
    
    // Update time
    const timeElement = document.getElementById('currentTime');
    if (timeElement) {
        const hours = now.getHours().toString().padStart(2, '0');
        const minutes = now.getMinutes().toString().padStart(2, '0');
        timeElement.textContent = `${hours}:${minutes}`;
    }
    
    // Update date
    const dateElement = document.getElementById('currentDate');
    if (dateElement) {
        const options = { month: 'short', day: 'numeric', year: 'numeric' };
        dateElement.textContent = now.toLocaleDateString('en-US', options);
    }
}

// Update every minute
updateDateTime();
setInterval(updateDateTime, 60000);

// Sidebar Toggle
const sidebarToggle = document.getElementById('sidebarToggle');
const mobileToggle = document.getElementById('mobileToggle');
const sidebar = document.getElementById('sidebar');
const mainContent = document.getElementById('mainContent');

if (sidebarToggle) {
    sidebarToggle.addEventListener('click', () => {
        sidebar.classList.toggle('collapsed');
        mainContent.classList.toggle('expanded');
    });
}

if (mobileToggle) {
    mobileToggle.addEventListener('click', () => {
        sidebar.classList.toggle('active');
    });
}

// Animated Counter for Metrics
function animateValue(element, start, end, duration) {
    let startTimestamp = null;
    const step = (timestamp) => {
        if (!startTimestamp) startTimestamp = timestamp;
        const progress = Math.min((timestamp - startTimestamp) / duration, 1);
        const value = Math.floor(progress * (end - start) + start);
        element.textContent = value.toLocaleString();
        if (progress < 1) {
            window.requestAnimationFrame(step);
        }
    };
    window.requestAnimationFrame(step);
}

// Animate all metric values on load
document.addEventListener('DOMContentLoaded', () => {
    const metricValues = document.querySelectorAll('.metric-value');
    metricValues.forEach(element => {
        const target = parseInt(element.getAttribute('data-target')) || parseInt(element.textContent.replace(/,/g, ''));
        if (target && !isNaN(target)) {
            animateValue(element, 0, target, 2000);
        }
    });
});

// Active Navigation Highlighting based on current URL
const navItems = document.querySelectorAll('.nav-item');
const currentPath = window.location.pathname;

navItems.forEach(item => {
    // Remove all active classes first
    item.classList.remove('active');
    
    // Add active class to current page
    if (item.getAttribute('href') === currentPath) {
        item.classList.add('active');
    }
});

// Refresh Button Animation
const refreshButtons = document.querySelectorAll('.btn-icon');
refreshButtons.forEach(btn => {
    btn.addEventListener('click', function(e) {
        if (this.querySelector('.fa-sync-alt')) {
            const icon = this.querySelector('.fa-sync-alt');
            icon.style.animation = 'spin 1s linear';
            setTimeout(() => {
                icon.style.animation = '';
            }, 1000);
        }
    });
});

// Add spin animation
const style = document.createElement('style');
style.textContent = `
    @keyframes spin {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
    }
`;
document.head.appendChild(style);

// Close sidebar on mobile when clicking outside
document.addEventListener('click', (e) => {
    if (window.innerWidth <= 768) {
        const sidebar = document.getElementById('sidebar');
        const mobileToggle = document.getElementById('mobileToggle');
        
        if (sidebar && sidebar.classList.contains('active')) {
            if (!sidebar.contains(e.target) && e.target !== mobileToggle) {
                sidebar.classList.remove('active');
            }
        }
    }
});