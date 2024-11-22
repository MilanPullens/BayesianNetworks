import requests
import os
import csv
import sys
import pandas as pd

os.environ["SPOTIPY_CLIENT_ID"]='16e5fb166e9443c58923e3957b24458a'
os.environ["SPOTIPY_CLIENT_SECRET"]='27f3fa8a016d486fbe8db5b86ceba172'
os.environ["SPOTIPY_REDIRECT_URI"]='http://localhost:8888/callback'

def requestToken(auth_code: str) -> str:
    token_data = {
        'grant_type': 'authorization_code',
        'code': auth_code,
        'redirect_uri': 'http://localhost:8888/callback',
        'client_id': '16e5fb166e9443c58923e3957b24458a',
        'client_secret': '27f3fa8a016d486fbe8db5b86ceba172'
    }
    token_resp = requests.post('https://accounts.spotify.com/api/token', data=token_data)
    return token_resp.json()['access_token']

def getTop50Tracks(token: str, offset:int) -> dict:
    headers = {'Authorization': f'Bearer {token}'}
    params = {'time_range': 'medium_term', 'limit': 50, 'offset': offset}
    return requests.get('https://api.spotify.com/v1/me/top/tracks', headers=headers, params=params).json()

access_token = open('auth_token.txt', 'r').read()

def getSimpleTrackInfos(token: str, idList:list[str]) -> dict:
    headers = {'Authorization': f'Bearer {token}'}
    params = {'ids': ','.join(idList)}
    return requests.get('https://api.spotify.com/v1/tracks', headers=headers, params=params).json()

def getTrackInfos(token: str, idList:list[str]) -> dict:
    headers = {'Authorization': f'Bearer {token}'}
    params = {'ids': ','.join(idList)}
    return requests.get('https://api.spotify.com/v1/audio-features', headers=headers, params=params).json()

def getArtistGenres(token: str, idList:list[str]) -> dict:
    headers = {'Authorization': f'Bearer {token}'}
    params = {'ids': ','.join(idList)}
    return requests.get('https://api.spotify.com/v1/artists', headers=headers, params=params).json()

def buildDatabase() -> dict:
    dataBase = {}
    for i in range(0,200,50):
        idList = []
        topTracks = getTop50Tracks(access_token, i)

        for track in topTracks['items']:
            idList.append(track['id'])
            dataBase[track['id']] = {'artist': track['artists'][0]['name'], 'name':track['name'], 
                                    'album':track['album']['name'], 'release_date':track['album']['release_date'], 
                                    'popularity':track['popularity'], 'explicit':track['explicit']}

        trackInfos = getTrackInfos(access_token, idList)   
        for trackInfo in trackInfos['audio_features']:
            dataBase[trackInfo['id']] |= {'danceability': trackInfo['danceability'], 'energy': trackInfo['energy'], 
                                        'key': trackInfo['key'], 'loudness': trackInfo['loudness'], 'mode': trackInfo['mode'], 
                                        'speechiness': trackInfo['speechiness'], 
                                        'acousticness': trackInfo['acousticness'], 'instrumentalness': trackInfo['instrumentalness'], 
                                        'liveness': trackInfo['liveness'], 'valence': trackInfo['valence'], 'tempo': trackInfo['tempo'],
                                        'duration_ms': trackInfo['duration_ms'], 'time_signature': trackInfo['time_signature']}

    return dataBase

def writeDatabaseToCSV(dataBase:dict) -> None:
    fileSuffix = sys.argv[1]
    with open(f"top200{fileSuffix}.csv", 'w', newline='', encoding='utf-8') as outFile:
        dataBaseCSV = csv.DictWriter(outFile, fieldnames=['id'] + list(list(dataBase.values())[0].keys()))
        dataBaseCSV.writeheader() 
        for id, track in dataBase.items():
            formattedRow = {'id':id}
            formattedRow |= track
            dataBaseCSV.writerow(formattedRow)

def addArtistInfos(dfPath:str) -> None:
    df = pd.read_csv(dfPath)
    top200Ids = list(df['id'])
    df['genres'] = ""
    for i in range(0,200,50):
        artistIds = []
        artistGenres = []
        trackInfos = getSimpleTrackInfos(access_token, top200Ids[i:i+50])
        for track in trackInfos['tracks']:
            artistIds.append(track['artists'][0]['id'])
        
        artistInfos = getArtistGenres(access_token, artistIds)

        for j, artist in enumerate(artistInfos['artists']):
            df.at[i+j, 'genres'] = ';'.join(artist['genres'])

    df.to_csv(dfPath, index=False, encoding='utf-8')


for name in ['Milan', 'Pip', 'Manon']:
    addArtistInfos(f"top200{name}.csv")   
# writeDatabaseToCSV(buildDatabase())