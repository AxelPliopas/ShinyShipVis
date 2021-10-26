Author: Adriano Axel Pliopas Pereira
Email: adriano.axel@gmail.com
Date: 25/10/2021

This Shiny application was created as an activity for Appsilion recruitment process and can be used only for the purpose of evaluating the candidate.

Details of the application, files, and main comments:

1. The files for data preparation are not included. Please request to adriano.axel@gmail.com in case they are necessary.

2. Regarding the data preparation, what was done was:

   - data was reordered by date, grouped by each SHIP_ID (there are a few cases of the same SHIP_ID being related to more than one ship name. In professional context I would investigate this futher, but for the purpose of this exercise I ignored this issue).

   - distance was calculated between consecutive observations for each SHIP_ID, using custom function implemented with Haversine formula and also considered the ellipsoid geometry of the earth, for higher precision

   - a flag (dummy variable) was inserted to identify the points of maximum distance and the immediately precedent observation.

   - another variable identified different travel legs (parts of the dataset without big jumps in the time labels, meaning that the data is continuous within each lag. This was used just for reporting the number of discontinuities but could be used also for more precise speed calculation, to avoid errors where these different legs connect.

   - some statistics were calculated to be displayed as each ship is selected: the proportion of observations classified as "parked" for each ship, and the time interval between the first and last observation for each ship.

