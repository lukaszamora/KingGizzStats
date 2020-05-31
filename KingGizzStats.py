import spotipy
from spotipy.oauth2 import SpotifyClientCredentials
import pandas as pd
import numpy as np
import matplotlib as plt
import time

# authenticate and connect to the API
client_id = '98b812809bc24988b825357a3c4b05a1'
client_secret = 'ae852286b5554e4381bb997988cfe8b0'
client_credentials_manager = SpotifyClientCredentials(
    client_id=client_id, client_secret=client_secret)  # spotify object to access API
sp = spotipy.Spotify(client_credentials_manager=client_credentials_manager)


# get track ids from playlist
def getPlaylistTrackIDs(user, playlist_id):
    ids = []
    playlist = sp.user_playlist(user, playlist_id)
    for item in playlist['tracks']['items']:
        track = item['track']
        ids.append(track['id'])
    return ids


# created playlist of every king gizz song (studio albums only) and use trackIDs from that playlist
ids = getPlaylistTrackIDs('lukas_zamora', '2MeunuArZ4CKBo4cp8z6us')


# get song info and audio analysis from song ids
def getTrackFeatures(id):
    meta = sp.track(id)
    features = sp.audio_features(id)

    # Meta
    name = meta['name']
    album = meta['album']['name']
    artist = meta['album']['artists'][0]['name']
    release_date = meta['album']['release_date']
    length = meta['duration_ms']
    popularity = meta['popularity']

    # Features
    acousticness = features[0]['acousticness']
    danceability = features[0]['danceability']
    energy = features[0]['energy']
    instrumentalness = features[0]['instrumentalness']
    key = features[0]['key']
    liveness = features[0]['liveness']
    loudness = features[0]['loudness']
    speechiness = features[0]['speechiness']
    tempo = features[0]['tempo']
    time_signature = features[0]['time_signature']
    valence = features[0]['valence']

    track = [name, album, artist, release_date, length, popularity, danceability, acousticness,
             danceability, energy, instrumentalness, key, liveness, loudness, speechiness, tempo, time_signature, valence]
    return track


# loop over track ids to create dataset
tracks = []
for i in range(0, len(ids)):
    # bug in Spotipy API only lets you loop through 100 songs...
    track = getTrackFeatures(ids[i])
    tracks.append(track)

# create track data frame and csv file
df = pd.DataFrame(tracks, columns=['name', 'album', 'artist', 'release_date', 'length', 'popularity', 'danceability', 'acousticness',
                                   'danceability', 'energy', 'instrumentalness', 'key', 'liveness', 'loudness', 'speechiness', 'tempo',
                                   'time_signature', 'valence'])
df.to_csv(r'C:\Users\lukas\Documents\KingGizzStats1.csv', sep=',')

print("Created CSV file.")
