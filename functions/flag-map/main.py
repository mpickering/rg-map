from flask import abort
from flask import make_response
from google.cloud import storage

storage_client = storage.Client()

def flag_map(request):
    if (request.method == 'OPTIONS'):
        r = make_response("")
        r.headers['Access-Control-Allow-Origin'] = "*"
        r.headers["Access-Control-Allow-Methods"] ="GET, POST, PATCH, PUT, DELETE, OPTIONS"
        r.headers["Access-Control-Allow-Headers"] = "Origin, Content-Type, X-Auth-Token"
        r.headers['Access-Control-Max-Age'] = "3600"
        return r

    request_json = request.get_json()

    bucket = storage_client.get_bucket("flagged-maps")
    destination_blob_name = request_json['hash']
    blob = bucket.blob(destination_blob_name)

    blob.upload_from_string("")
    r = make_response()
    r.headers['Access-Control-Allow-Origin'] = "*"
    r.headers["Access-Control-Allow-Methods"] ="GET, POST, PATCH, PUT, DELETE, OPTIONS"
    r.headers["Access-Control-Allow-Headers"] = "Origin, Content-Type, X-Auth-Token"
    r.headers['Access-Control-Max-Age'] = "3600"
    return r
