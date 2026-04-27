from app import app, db
from models import User
from werkzeug.security import generate_password_hash

def init_database():
    """Initialize the database and create default users"""
    with app.app_context():
        # Drop all tables and recreate them
        db.drop_all()
        db.create_all()
        
        # Create admin user
        admin = User(
            username='admin',
            password=generate_password_hash('admin123')
        )
        
        # Create test user
        user1 = User(
            username='user1',
            password=generate_password_hash('password123')
        )
        
        # Add users to database
        db.session.add(admin)
        db.session.add(user1)
        db.session.commit()
        
        print("✓ Database initialized successfully!")
        print("\n--- Default Users Created ---")
        print("1. Username: admin    | Password: admin123")
        print("2. Username: user1    | Password: password123")
        print("\nYou can now run 'python app.py' to start the server")

if __name__ == '__main__':
    init_database()
