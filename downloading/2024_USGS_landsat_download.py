# %%
# Download packages
import json
import requests
from getpass import getpass
import sys
import time
import re
import threading
import datetime
import os
import pandas as pd
import geopandas as gpd
import pyproj
from shapely.geometry import box
from shapely import wkt



import warnings
warnings.filterwarnings("ignore")

# %% [markdown]
# Code adapted from USGS M2M tutorial: https://code.usgs.gov/eros-user-services/machine_to_machine/using-checksums-with-m2m/-/blob/main/Using_M2M_Checksums_v2.ipynb?ref_type=heads

# %% [markdown]
# Define key functions

# %%
# Send HTTP request to connect to server 
def sendRequest(url, data, apiKey=None, exitIfNoResponse=True):
    """
    Send a request to an M2M (Machine-to-Machine) endpoint and return the parsed JSON response.

    Parameters:
    - url (str): The URL of the M2M endpoint.
    - data (dict): The payload to be sent with the request.
    - apiKey (str, optional): An optional API key for authorization. If not provided, the request will be sent without an authorization header.
    - exitIfNoResponse (bool, optional): If True, the program will exit upon receiving an error or no response. Defaults to True.

    Returns:
    - dict: The parsed JSON response containing the data, or False if there was an error.
    """
    
    # Convert payload to json string
    json_data = json.dumps(data)
    
    if apiKey == None:
        response = requests.post(url, json_data)
    else:
        headers = {'X-Auth-Token': apiKey}              
        response = requests.post(url, json_data, headers = headers)  
    
    try:
      httpStatusCode = response.status_code 
      if response == None:
          print("No output from service")
          if exitIfNoResponse: sys.exit()
          else: return False
      output = json.loads(response.text)
      if output['errorCode'] != None:
          print(output['errorCode'], "- ", output['errorMessage'])
          if exitIfNoResponse: sys.exit()
          else: return False
      if  httpStatusCode == 404:
          print("404 Not Found")
          if exitIfNoResponse: sys.exit()
          else: return False
      elif httpStatusCode == 401: 
          print("401 Unauthorized")
          if exitIfNoResponse: sys.exit()
          else: return False
      elif httpStatusCode == 400:
          print("Error Code", httpStatusCode)
          if exitIfNoResponse: sys.exit()
          else: return False
    except Exception as e: 
          response.close()
          print(e)
          if exitIfNoResponse: sys.exit()
          else: return False
    response.close()
    
    return output['data']

# %%
serviceUrl = "https://m2m.cr.usgs.gov/api/api/json/stable/" 

# %%
def downloadFile(url, out_dir):
    sema.acquire()
    try:
        response = requests.get(url, stream=True)
        disposition = response.headers['content-disposition']
        filename = re.findall("filename=(.+)", disposition)[0].strip("\"")
        print(f"    Downloading: {filename} -- {url}...")
        
        open(os.path.join(out_dir, filename), 'wb').write(response.content)
        sema.release()
    except Exception as e:
        print(f"\nFailed to download from {url}. Will try to re-download.")
        sema.release()
        runDownload(threads, url, out_dir)

# %%
def generate_sha(filepath, checksum_type):
    try:
        if checksum_type == 'sha512':
            sha512 = hashlib.sha512()
            BUF_SIZE = 65536

            with open(filepath, 'rb') as f:
                while True:
                    data = f.read(BUF_SIZE)
                    if not data:
                        break
                    sha512.update(data)
                    
            local_sha = sha512.hexdigest()
            return local_sha

    except:
        print(f'Error Generating Checksum for file: {filepath}')

        return False

# %%
def downloadFile(download_rq_result, out_dir):
    sema.acquire()
    url = download_rq_result['url']
    try:
        response = requests.get(url, stream=True)
        disposition = response.headers['content-disposition']
        filename = re.findall("filename=(.+)", disposition)[0].strip("\"")
        print(f"> Downloading: {filename} -- {url}...")

        # Save file in out directory
        open(os.path.join(out_dir, filename), 'wb').write(response.content)

        # Landsat Products currently have SHA-512 checksums
        checksum_type = download_rq_result['checksum_values'][0]['id']
        print(checksum_type)
        if 'checksum_values' in download_rq_result:
            
            # Generate the checksum from the downloaded file
            generated_checksum = generate_sha(os.path.join(out_dir, filename), checksum_type)
            checksum_values = []
            checksum_values.append([checksum_val['value'] for checksum_val in download_rq_result['checksum_values']])
            
            if (any( generated_checksum in c for c in checksum_values)):
                print(f"    Checksum validation PASSED for {filename}.")
                print(f"    {checksum_type}: {generated_checksum}")
            else:
                print(f"    !Checksum validation FAILED for {filename}")
        else:
            print(f"    !No checksum values returned from download request for {filename}.")
        
        
        sema.release()
    except Exception as e:
        print(f"    !!Failed to download {filename} -- {url}. Will try to re-download.")
        sema.release()
        runDownload(threads,download_rq_result, out_dir)

# %%
def runDownload(threads, download_rq_result, out_dir):
    thread = threading.Thread(target=downloadFile, args=(download_rq_result,out_dir,))
    threads.append(thread)
    thread.start()

# %%
maxthreads = 5 # Threads count for downloads
sema = threading.Semaphore(value=maxthreads)
label = datetime.datetime.now().strftime("%Y%m%d_%H%M%S") # Customized label using date time
threads = []

# %%
def runDownload(threads, url, out_dir):
    thread = threading.Thread(target=downloadFile, args=(url,out_dir,))
    threads.append(thread)
    thread.start()

# %%
# Set output directory
out_dir = ("/u/home/h/hstouter/project-eordway/LCLUC_data/landsat_tests_300km/Landsat_USGS/2024")
os.chdir(out_dir)  # switch to that directory
print(out_dir)


# %%
# Set multithreading parameters
maxthreads = 5 # Threads count for downloads
sema = threading.Semaphore(value=maxthreads)
label = datetime.datetime.now().strftime("%Y%m%d_%H%M%S") # Customized label using date time
threads = []

# %% [markdown]
# Connect to USGS

# %%
username = "hstouter"
token = "H3xVeg!r!zyZoWtWqt2@uvIKIJVZ_ntvs89MIMLFMj6HRZWhewdPTor9C5iXQ@Ez"


login_payload = {'username' : username, 'token' : token}
apiKey = sendRequest(serviceUrl + "login-token", login_payload)
print("API Key: " + apiKey + "\n")

# %% [markdown]
# Define ROI from .shp file

# %%
# to get correct CRS
# pyproj.datadir.set_data_dir("/Users/hstouter/anaconda3/envs/remote_sensing/share/proj")

# Study area shp
shp_path = ("/u/home/h/hstouter/project-eordway/fire/shp_files/Dja_300km_buffer_envelope.shp")
shp = gpd.read_file(shp_path)
print(shp.head())

# Check CRS & update if needed
print("original :", shp.crs)

# Update CRS
shp = shp.to_crs(epsg=4326)
print("updated :", shp.crs)

# Get bounding box coordinates
minx, miny, maxx, maxy = shp.total_bounds

# Create a shapely box (polygon)
bbox = box(minx, miny, maxx, maxy)

# Fix geometry (ensure it's valid and CCW)
bbox_fixed = bbox.buffer(0)


# Convert to GeoJSON-like dict
roi = bbox_fixed.__geo_interface__

# Wrap in API spatialFilter format
roi = {
    'filterType': 'geojson',
    'geoJson': roi
}

print(roi)

# %% [markdown]
# Define JSON inputs

# %%
datasetName = 'landsat_ot_c2_l2'
spatialFiler = roi
aquisitionFilter = {'start' : '2024-10-01', 'end' : '2025-09-30'}
cloudCoverFilter = {'min' : 0, 'max' : 80}

# %%
search_payload = {
    'datasetName' : datasetName,
    'sceneFilter' : {
        'spatialFilter' : spatialFiler,
        'acquisitionFilter' : aquisitionFilter,
        'cloudCoverFilter' : cloudCoverFilter
    }
}

search_payload

# %%
# Create scene ID list
{'listId': 'temp_landsat_ot_c2_l2_list',
     'idField': 'entityId',
     'entityIds': ['LC08_L2SP_068017_20200310_20200822_02_T1', 'LC08_L2SP_068018_20200310_20200822_02_T1'],
     'datasetName': 'landsat_ot_c2_l2'}

# %%
# Pull in csv with all landsat scenes and convert to scene list
df = pd.read_csv(
    "/u/home/h/hstouter/project-eordway/LCLUC_data/landsat_tests_300km/Landsat_USGS/scene_ids/2024_landsat_ot_c2_l2_695f3449892201d9.csv",
    encoding="latin1"
)

print("Number of rows df:", len(df))

# Pull column 
ls_scenes = df["Landsat Scene Identifier"]
print(ls_scenes.head())

# Convert same format as searching through lists
ls_scenes = (
    df["Landsat Scene Identifier"]
    .astype(str)
    .str.strip()
    .tolist()
)

ls_scenes

print("Number of scenes:", len(ls_scenes))

# %%
idField = 'entityId'
listId = f"temp_{datasetName}_list"

scn_list_add_payload = {
    "listId": listId,
    'idField' : idField,
    "entityIds": ls_scenes,
    "datasetName": "landsat_ot_c2_l2"
}

print("Number of scenes:", len(ls_scenes))
scn_list_add_payload

# %%
# Check the number of scenes added to the request
count = sendRequest(serviceUrl + "scene-list-add", scn_list_add_payload, apiKey) 
count

# %%
# view the items in the scene list
sendRequest(serviceUrl + "scene-list-get", {'listId' : scn_list_add_payload['listId']}, apiKey)  

# %%
# Define the bands you want to download 
bandNames = {'SR_B1', 'SR_B2', 'SR_B3', 'SR_B4', 'SR_B5', 'SR_B6','SR_B7', 'ST_B10','QA_PIXEL'} 

# %% [markdown]
# Prepare the download request

# %%
# Prepare the payload for the download options request
download_opt_payload = {
    "listId": listId,              
    "datasetName": datasetName      
}

# Print the payload for debugging purposes
print(f"download_opt_payload: {download_opt_payload}")

# Send request to the download options endpoint and retrieve list of available products
products = sendRequest(serviceUrl + "download-options", download_opt_payload, apiKey)
pd.json_normalize(products)

# %%
download_opt_payload: {'listId': 'temp_landsat_etm_c2_l2_list', 'datasetName': 'landsat_etm_c2_l2'}


# %%
# Get all available products 

downloads = []
for product in products:  
    if product["secondaryDownloads"] is not None and len(product["secondaryDownloads"]) > 0:
        for secondaryDownload in product["secondaryDownloads"]:
            for bandName in bandNames:
                if secondaryDownload["bulkAvailable"] and bandName in secondaryDownload['displayId']:
                    downloads.append({"entityId":secondaryDownload["entityId"], "productId":secondaryDownload["id"]})

# %% [markdown]
# Submit the download request

# %%
download_req_payload = {
        "downloads": downloads,
        "label": label
    }

download_req_payload

# %%
 # Send the download request using the provided payload and store the results
download_request_results = sendRequest(serviceUrl + "download-request", download_req_payload, apiKey)

# Check if any new records or duplicate products were returned
if len(download_request_results['newRecords']) == 0 and len(download_request_results['duplicateProducts']) == 0:
    print('No records returned, please update your scenes or scene-search filter')
    sys.exit()

# %%
print(download_request_results) 

# %%
for dl in download_request_results['availableDownloads']:
    print(f"{dl['entityId']}: \n {dl['checksum_values']}")

# %% [markdown]
# Download the request

# %%
def run_download_retrieve(download_request_results, out_dir):
    
    # Attempt to download URLs if available
    if len(download_request_results['availableDownloads']) > 0:
        print(f"Downloading {len(download_request_results['availableDownloads'])} files... Please do not close the program\n")
        for result in download_request_results['availableDownloads']:  
            # print(f"Get download url: {result['url']}\n" )
            runDownload(threads, result, out_dir)
    
    # Get items labeled as being prepared for Download
    elif len(download_request_results['preparingDownloads']) > 0:
        print(f"Preparing Downloads for {len(download_request_results['preparingDownloads'])} files... Please do not close the program\n")
    
        preparingDownloadIds = []
    
        for result in download_request_results['preparingDownloads']:  
            preparingDownloadIds.append(result['downloadId'])
    
        download_ret_payload = {"label" : download_req_payload['label']}                
        # Retrieve download URLs
        print("Retrieving download urls...\n")
        download_retrieve_results = sendRequest(serviceUrl + "download-retrieve", download_ret_payload, apiKey, False)
        print(f"download_retrieve_results: {download_retrieve_results}")
        if download_retrieve_results != False:
            print(f"    Download-retrieve complete: \n" )
            for result in download_retrieve_results['available']:
                if result['downloadId'] in preparingDownloadIds:
                    preparingDownloadIds.remove(result['downloadId'])
                    runDownload(threads, result, out_dir)
                    print(f"       {result['url']}\n" )
    
            for result in download_retrieve_results['requested']:   
                if result['downloadId'] in preparingDownloadIds:
                    preparingDownloadIds.remove(result['downloadId'])
                    runDownload(threads, result, out_dir)
                    print(f"       {result['url']}\n" )
    
        # Didn't get all download URLs, retrieve again after 30 seconds
        while len(preparingDownloadIds) > 0: 
            print(f"{len(preparingDownloadIds)} downloads are not available yet. Waiting for 30s to retrieve again\n")
            time.sleep(30)
            download_retrieve_results = sendRequest(serviceUrl + "download-retrieve", download_ret_payload, apiKey, False)
            if download_retrieve_results != False:
                for result in download_retrieve_results['available']:                            
                    if result['downloadId'] in preparingDownloadIds:
                        preparingDownloadIds.remove(result['downloadId'])
                        print(f"    Get download url: {result['url']}\n" )
                        runDownload(threads, result, out_dir)
    
    for thread in threads:
        thread.join()        

# %%
run_download_retrieve(download_request_results, out_dir) 

# %% [markdown]
# Log out

# %%
endpoint = "logout"  
if sendRequest(serviceUrl + endpoint, None, apiKey) == None:        
    print("\nLogged Out\n")
else:
    print("\nLogout Failed\n")


