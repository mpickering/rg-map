import sendgrid
import os
from sendgrid.helpers.mail import *

from google.cloud import storage
import urllib.request
import json
import staticmap
from PIL import Image


storage_client = storage.Client()


def send_verification_email(data, context):
    """Background Cloud Function to be triggered by Cloud Storage.
       This generic function logs relevant data when a file is changed.

    Args:
        data (dict): The Cloud Functions event payload.
        context (google.cloud.functions.Context): Metadata of triggering event.
    Returns:
        None; the output is written to Stackdriver Logging
    """
    manifest_blob = storage_client.get_bucket("rg-maps-raw").blob("manifest.json")
    gcs_file = manifest_blob.download_as_string()
    manifest = json.loads(gcs_file)
    manifest = { m["hash"]: m for m in manifest }

    h, _ = os.path.splitext(data['name'])



    event = manifest[h]


    # Write map and world file to /tmp
    wf_blob = storage_client.get_bucket("rg-maps-world-files").blob(data['name'])
    wf_blob.download_to_filename("/tmp/map.jgw")

    urllib.request.urlretrieve(event['map_url'], "/tmp/map.jpg")






    print('Event ID: {}'.format(context.event_id))
    print('Event type: {}'.format(context.event_type))
    print('Bucket: {}'.format(data['bucket']))
    print('File: {}'.format(data['name']))
    print('Metageneration: {}'.format(data['metageneration']))
    print('Created: {}'.format(data['timeCreated']))
    print('Updated: {}'.format(data['updated']))


    map = staticmap.StaticMap(3000, 4000, 7)

    with open("/tmp/map.jgw") as f:
        content = f.readlines()
    # you may also want to remove whitespace characters like `\n` at the end of each line
    gt = [float(x.strip()) for x in content]
    print(gt)

    im = Image.open("/tmp/map.jpg")
    cols, rows = im.size


    p, rot = staticmap.MakePolygon(gt, cols, rows)
    print(p)
    print(p.bounds)
    print(p.envelope.bounds)
    print(p.envelope)

    #map.add_polygon(Polygon(p.exterior.coords))

    #map.add_polygon(Polygon(p.envelope.exterior.coords, fill_color="red"))

#
    test_image = im.convert("RGBA").rotate(rot, expand=True)

    gi = staticmap.Geoimage(p.bounds, test_image)
    map.add_image(gi)

    image = map.render()
    image.save("/tmp/image.png")
    bucket = storage_client.get_bucket("verif-images")
    blob = bucket.blob(h + ".png")

    blob.upload_from_filename("/tmp/image.png")


    #sg = sendgrid.SendGridAPIClient(apikey=os.environ.get('SENDGRID_API_KEY'))
    #from_email = Email("maps@mpickering.github.io")
    #to_email = Email("matthewtpickering@gmail.com")
    #subject = "Sending with SendGrid is Fun"
    #content = Content("text/plain", "and easy to do anywhere, even with Python")
    #mail = Mail(from_email, subject, to_email, content)
    #response = sg.client.mail.send.post(request_body=mail.get())
    #print(response.status_code)
    #print(response.body)
    #print(response.headers)
