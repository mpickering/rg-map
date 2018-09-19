from google.cloud import storage

storage_client = storage.Client()

def do_verification(request):
    decision = request.args['decision']
    h = request.args['hash']
    print(decision, h)

    wf_bucket = storage_client.get_bucket("rg-maps-world-files")
    verif_bucket = storage_client.get_bucket("verif-images")
    final_bucket = storage_client.get_bucket("rg-maps-final-world-files")

    wf_name = h + ".jgw"
    v_map_name   = h + ".png"

    wf = wf_bucket.blob(wf_name)
    vmap = verif_bucket.blob(v_map_name)

    if decision == "accept":
        # Copy the blob over
        wf_bucket.copy_blob(wf, final_bucket, wf_name)
    # Delete the image and world file
    wf.delete()
    # Always delete the verification map, we don't need it anymore
    vmap.delete()
    return "Success"
