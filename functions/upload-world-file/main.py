from flask import abort
from flask import make_response
from flask_cors import CORS, cross_origin
from google.cloud import storage
import jsonschema

storage_client = storage.Client()

worldFileSchema = {
        "definitions" : {
            "world_file" : {
                "type" : "object",
                "properties" : { key : {"type" : "number"}  for key in ["A","B","C","D","E", "F"]},
                "required" : ["A", "B", "C", "D", "E", "F"]
            }
        },
        "type": "object",
        "properties": {
            "world_file": { "$ref": "#definitions/world_file"},
            "hash" : { "type": "string" }
        },
        "required" : ["world_file","hash"]
    }



def upload_world_file(request):
    if (request.method == 'OPTIONS'):
        r = make_response("")
        r.headers['Access-Control-Allow-Origin'] = "*"
        r.headers["Access-Control-Allow-Methods"] ="GET, POST, PATCH, PUT, DELETE, OPTIONS"
        r.headers["Access-Control-Allow-Headers"] = "Origin, Content-Type, X-Auth-Token"
        r.headers['Access-Control-Max-Age'] = "3600"
        return r

    request_json = request.get_json()
    jsonschema.validate(request_json, worldFileSchema)

    bucket = storage_client.get_bucket("rg-maps-world-files")
    destination_blob_name = request_json['hash'] + ".jgw"
    blob = bucket.blob(destination_blob_name)

    f = "\n".join(["%.30f" % request_json['world_file'][index] for index in ['A', 'D', 'B', 'E', 'C', 'F']])

    blob.upload_from_string(f)
    r = make_response(str(request_json))
    r.headers['Access-Control-Allow-Origin'] = "*"
    r.headers["Access-Control-Allow-Methods"] ="GET, POST, PATCH, PUT, DELETE, OPTIONS"
    r.headers["Access-Control-Allow-Headers"] = "Origin, Content-Type, X-Auth-Token"
    r.headers['Access-Control-Max-Age'] = "3600"
    return r
