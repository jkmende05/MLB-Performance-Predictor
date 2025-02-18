# MLB-Performance-Predictor
This project uses R and Machine Learning Techniques to determine future player stats based on historical data.
Two different techniques are used. The first is a clustering method that combines current players to
past players. It will also apply an age adjustment to the stats based on the player's age. Players within their prime 
will see a slight increase in their performance, while older players will see a slight decrease. The second 
technique uses XGBoost to train a model and test it, before providing predictions for the players. The predictions from
both methods are then averaged to get the final predictions. Currently, only predictions are made for hitters.