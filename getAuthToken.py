import os
import random
import string
from flask import Flask, redirect, request
import requests
import urllib.parse
import webbrowser
import threading
import time
import signal
import sys

os.environ["SPOTIPY_CLIENT_ID"]='16e5fb166e9443c58923e3957b24458a'
os.environ["SPOTIPY_CLIENT_SECRET"]='27f3fa8a016d486fbe8db5b86ceba172'
os.environ["SPOTIPY_REDIRECT_URI"]='http://localhost:8888/callback'

app = Flask(__name__)

def generate_random_string(length):
    return ''.join(random.choices(string.ascii_letters + string.digits, k=length))

@app.route('/login')
def login():
    state = generate_random_string(16)
    scope = 'user-top-read'

    auth_url = 'https://accounts.spotify.com/authorize?' + urllib.parse.urlencode({
        'response_type': 'code',
        'client_id': os.getenv('SPOTIPY_CLIENT_ID'),
        'scope': scope,
        'redirect_uri': os.getenv('SPOTIPY_REDIRECT_URI'),
        'show_dialog':True,
        'state': state
    })

    return redirect(auth_url)

@app.route('/callback')
def callback():
    code = request.args.get('code')
    state = request.args.get('state')

    token_url = 'https://accounts.spotify.com/api/token'
    token_data = {
        'grant_type': 'authorization_code',
        'code': code,
        'redirect_uri': os.getenv('SPOTIPY_REDIRECT_URI'),
        'client_id': os.getenv('SPOTIPY_CLIENT_ID'),
        'client_secret': os.getenv('SPOTIPY_CLIENT_SECRET')
    }

    token_response = requests.post(token_url, data=token_data)
    token_info = token_response.json()

    access_token = token_info.get('access_token')

    with open('auth_token.txt', 'w') as authTokenFile:
        authTokenFile.write(access_token)
    
    authTokenFile.close()

    return f"Printed:\n{access_token}\n to the auth_token.txt file."

def open_browser():
    webbrowser.open_new("http://127.0.0.1:8888/login")

def run_flask():
    try:
        app.run(port=8888, use_reloader=False)
    except KeyboardInterrupt as e:
        sys.exit(0)

if __name__ == '__main__':
    open_browser()
    run_flask()