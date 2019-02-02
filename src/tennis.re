type player =
  | PlayerOne
  | PlayerTwo;

type point =
  | Love
  | Fifteen
  | Thirty;

type pointsData = {
  playerOne: point,
  playerTwo: point,
};

type fortyData = {
  player, /* The player who have forty points */
  otherPlayerPoint: point,
};

type score =
  | Points(pointsData)
  | Forty(fortyData)
  | Deuce
  | Advantage(player)
  | Game(player);

let s1 = {playerOne: Love, playerTwo: Love};
let s2 = {playerOne: Fifteen, playerTwo: Love};
let s3 = {playerOne: Thirty, playerTwo: Love};
let fd: fortyData = {player: PlayerOne, otherPlayerPoint: Love};

let startScore: score = Points({playerOne: Love, playerTwo: Love});
let anotherScore: score = Forty({player: PlayerTwo, otherPlayerPoint: Thirty});

let scoreWhenDeuce: player => score = winner => Advantage(winner);

let scoreWhenAdvantage: (player, player) => score =
  (advantagedPlayer, winner) => advantagedPlayer == winner ? Game(winner) : Deuce;

/* This time we infer that the function type is (player) => player */
let other = player =>
  switch (player) {
  | PlayerOne => PlayerTwo
  | PlayerTwo => PlayerOne
  };

/* We add a tool function to increment point */
let incrementPoint: point => option(point) =
  point =>
    switch (point) {
    | Love => Some(Fifteen)
    | Fifteen => Some(Thirty)
    | Thirty => None
    };

let scoreWhenForty = (current, winner) =>
  current.player == winner ?
    Game(winner) :
    (
      switch (incrementPoint(current.otherPlayerPoint)) {
      | Some(p) => Forty({...current, otherPlayerPoint: p})
      | None => Deuce
      }
    );

let pointTo = (player, point, current) =>
  switch (player) {
  | PlayerOne => {...current, playerOne: point}
  | PlayerTwo => {...current, playerTwo: point}
  };

let pointFor = (player, current) =>
  switch (player) {
  | PlayerOne => current.playerOne
  | PlayerTwo => current.playerTwo
  };

let scoreWhenPoints = (current, winner) =>
  switch (current |> pointFor(winner) |> incrementPoint) {
  | Some(np) => Points(pointTo(winner, np, current))
  | None => Forty({player: winner, otherPlayerPoint: current |> pointFor(other(winner))})
  };

let scoreWhenGame = winner => Game(winner);

let score = (current, winner) =>
  switch (current) {
  | Points(p) => scoreWhenPoints(p, winner)
  | Forty(f) => scoreWhenForty(f, winner)
  | Deuce => scoreWhenDeuce(winner)
  | Advantage(a) => scoreWhenAdvantage(a, winner)
  | Game(g) => scoreWhenGame(g)
  };

let newGame = Points({playerOne: Love, playerTwo: Love});

let string_of_point = point =>
  switch (point) {
  | Love => "Love"
  | Fifteen => "15"
  | Thirty => "30"
  };

let string_of_player = player =>
  switch (player) {
  | PlayerOne => "PlayerOne"
  | PlayerTwo => "PlayerTwo"
  };

let string_of_score = score =>
  switch (score) {
  | Points(pd) =>
    "PlayerOne has "
    ++ string_of_point(pd.playerOne)
    ++ " pts and PlayerTwo has "
    ++ string_of_point(pd.playerTwo)
    ++ " pts"
  | Forty(fd) =>
    string_of_player(fd.player) ++ " has 40 pts. Other player has " ++ string_of_point(fd.otherPlayerPoint) ++ " pts"
  | Deuce => "Deuce"
  | Advantage(p) => string_of_player(p) ++ " has the advantage"
  | Game(g) => string_of_player(g) ++ " won!"
  };
