Hello all!

Check the 'Paper' branch for a separate report on some new models that will be integrated into this app.

# App Functionality
As of now, the app can do the following:
- Add and view notes about sessions 
- View Pitch Velocity, Spin, and Break over Time

## To-Be-Implemented
The app *will* also be able to do the following:
- View Strike-zone plots annotated with whiff and called-strike probabilities

# Pros 
The app is integrated with AWS, and so uploading additional game files or bullpen files into this application is as easy as uploading files to the service.

**Note**: A private key and access ID are required to use AWS. Let me know if you want to use this locally, and I can send that information.

Additionally, as we add game and scrimmage data, the 'whiffScript.sh' file in the 'CreateModels' directory allows for a quick and easy update of the model. A simple double-click, and all models are trained anew!

# Cons
To perform better, the models in the app will need more data. *Give the app time!* :)

