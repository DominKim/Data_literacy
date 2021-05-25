# -*- coding: utf-8 -*-
import pandas
import json
from googleapiclient.discovery import build

# test one video
# json 파일 경로를 넣고싶은데 어떻게 넣어야할까요?...
api_key = "./config/api_key.json"
video_id = "./config/videos.json"

comments = list()
api_obj = build('youtube', 'v3', developerKey=api_key)
response = api_obj.commentThreads().list(part='snippet,replies', videoId=video_id, maxResults=100).execute()

while response:
    for item in response['items']:
        comment = item['snippet']['topLevelComment']['snippet']
        comments.append(
         [
             comment['textDisplay'],
             comment['authorDisplayName'],
             comment['publishedAt'],
             comment['likeCount']
         ]   
        )
        if item['snippet']['totalReplyCount'] > 0:
            for reply_item in item['replies']['comments']:
                reply = reply_item['snippet']
                comments.append(
                    [
                        reply['textDisplay'],
                        reply['authorDisplayName'],
                        reply['publishedAt'],
                        reply['likeCount']
                    ]
                )
    if 'nextPageToken' in response:
        response = api_obj.commentThreads().list(part='snippet,replies', videoId=video_id, pageToken=response['nextPageToken'], maxResults=100).execute()
    else:
        break
df = pandas.DataFrame(comments)
df.to_csv('results.csv', header=['comment', 'auther', 'date', 'num_likes'], index=None, encoding='utf-8-sig')
