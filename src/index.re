open Reprocessing;

type pixelT =
  | Empty
  | Fill(colorT);

type gameStateT = Playing | Gameover

type stateT = {
  player: (int, int),
  matrix: array(array(pixelT)),
  shape: list(list(pixelT)),
  nextShape: list(list(pixelT)),
  keyUp: bool,
  keyDown: bool,
  keyLeft: bool,
  keyRight: bool,
  elapsed: int,
  score: int,
  font: fontT,
  gameState: gameStateT
};

let square = (pixel: pixelT) => [[pixel, pixel], [pixel, pixel]];
let line = (pixel: pixelT) => [[pixel, pixel, pixel, pixel]];
let tShape = (pixel: pixelT) => [
  [pixel, pixel, pixel],
  [Empty, pixel, Empty],
];
let lShape = (pixel: pixelT) => [
  [pixel, pixel, pixel],
  [pixel, Empty, Empty],
];
let zShape = (pixel: pixelT) => [
  [pixel, pixel, Empty],
  [Empty, pixel, pixel],
];

let colors = [
  Utils.color(~r=69, ~g=205, ~b=255, ~a=255),
  Utils.color(~r=73, ~g=232, ~b=62, ~a=255),
  Utils.color(~r=255, ~g=212, ~b=50, ~a=255),
  Utils.color(~r=232, ~g=73, ~b=126, ~a=255),
  Utils.color(~r=178, ~g=67, ~b=255, ~a=255),
];

let backgroundColor = Utils.color(~r=42, ~g=42, ~b=42, ~a=255);
let asideColor = Utils.color(~r=106, ~g=117, ~b=127, ~a=255);
let getRandomShape = () => {
  let color = List.nth(colors, Random.int(List.length(colors)));
  switch (Random.int(4)) {
  | 0 => square(Fill(color))
  | 1 => line(Fill(color))
  | 2 => lShape(Fill(color))
  | 3 => tShape(Fill(color))
  | _ => zShape(Fill(color))
  };
};

let matrixHeight = 28;
let matrixWidth = 20;
let pixelSize = 30;
let delay = 20;

let asideWidth = 230;

let drawMatrix = ({matrix}, env) =>
  Array.iteri(
    (y, row) =>
      Array.iteri(
        (x, pixel) =>
          switch (pixel) {
          | Empty => Draw.fill(backgroundColor, env)
          | Fill(color) =>
            Draw.fill(color, env);
            Draw.rect(
              ~pos=(x * pixelSize, y * pixelSize),
              ~width=pixelSize,
              ~height=pixelSize,
              env,
            );
          },
        row,
      ),
    matrix,
  );

let drawShape = (~shape, ~relative, env) =>
  List.iteri(
    (y, row) =>
      List.iteri(
        (x, pixel) =>
          switch (pixel) {
          | Fill(color) =>
            Draw.fill(color, env);
            let (relativeX, relativeY) = relative;
            let x = relativeX + x;
            let y = relativeY + y;

            Draw.rect(
              ~pos=(x * pixelSize, y * pixelSize),
              ~width=pixelSize,
              ~height=pixelSize,
              env,
            );

          | _ => Draw.fill(backgroundColor, env)
          },
        row,
      ),
    shape,
  );

let flip = matrix =>
  List.mapi(
    (i, _column) => List.map(row => List.nth(row, i), matrix),
    List.hd(matrix),
  );

let rotate = matrix => flip(List.rev(matrix));

let adjustPlayerAfterRotation = (matrix, player): int => {
  let (playerX, _) = player;

  if (playerX + (matrix -> List.hd -> List.length) - 1 > matrixWidth - 1) {
    playerX + (matrix -> List.hd -> List.length) - matrixWidth;
  } else {
    0
  }
}

let maxX = (x, shape) => { 
  let shapeWidth = shape -> List.hd -> List.length;
  x + shapeWidth - 1 >= matrixWidth ? matrixWidth - shapeWidth: x;
}
let minX = x => x < 0 ? 0 : x;

let isColapsing = (~player, ~shape, ~matrix) => {
  let found = ref(false);

  List.iteri(
    (y, row) =>
      List.iteri(
        (x, pixel) =>
          if (pixel != Empty) {
            let (playerX, playerY) = player;
            let x = playerX + x;
            let y = playerY + y;

            if (y >= matrixHeight - 1 || matrix[y + 1][x] != Empty) {
              found := true;
            };
          },
        row,
      ),
    shape,
  );

  found^;
};

let setShapeInMatrix = (~shape, ~player, ~matrix) =>
  Array.mapi(
    (y, row) =>
      Array.mapi(
        (x, pixel) => {
          let (playerX, playerY) = player;

          if (x >= playerX
              && y >= playerY
              && y
              - playerY <= shape->List.length
              - 1
              && x
              - playerX <= shape->List.nth(y - playerY)->List.length
              - 1
              && shape->List.nth(y - playerY)->List.nth(x - playerX) != Empty) {
            shape->List.nth(y - playerY)->List.nth(x - playerX);
          } else {
            pixel;
          };
        },
        row,
      ),
    matrix,
  );

let clearFullLines = (~matrix) => {
  let filteredRows = List.filter(
    row => List.exists(pixel => pixel == Empty, row->Array.to_list),
    matrix->Array.to_list,
  );

  let score = matrix->Array.length - filteredRows->List.length;
  let emptyLines = Array.make(score, Array.make(matrixWidth, Empty)) -> Array.to_list;
  (
    score,
    List.append(emptyLines, filteredRows) -> Array.of_list
  )
}

let getInitialState = (env) => {
  {
    player: (8, 0),
    shape: getRandomShape(),
    nextShape: getRandomShape(),
    matrix: Array.make_matrix(matrixHeight, matrixWidth, Empty),
    keyUp: false,
    keyDown: false,
    keyLeft: false,
    keyRight: false,
    elapsed: 0,
    score: 0,
    font: Draw.loadFont(~filename="assets/subway-ticker.fnt", ~isPixel=true, env), 
    gameState: Playing
  };
}

let setup = env => {
  Env.size(
    ~width=matrixWidth * pixelSize + asideWidth,
    ~height=matrixHeight * pixelSize,
    env,
  );
  getInitialState(env);
};

let drawSideBar = (state, env) => {
  Draw.rect(
      ~pos=(pixelSize * matrixWidth, 0),
      ~width=asideWidth,
      ~height=pixelSize * matrixHeight,
      env,
    );
    drawShape(~shape=state.nextShape, ~relative=(
      matrixWidth + 2,
      matrixHeight / 2
    ), env)  
    
    Draw.text(~font=state.font, ~body="Next Shape", ~pos=((matrixWidth * pixelSize + 20), matrixHeight * pixelSize / 2 - 50), env);
    Draw.pushMatrix(env);
    let textWidth = Draw.textWidth(~font=state.font, ~body=string_of_int(state.score), env);
    Draw.scale(~x=2., ~y=2., env);
    Draw.text(~font=state.font, ~body=string_of_int(state.score), ~pos=((matrixWidth * pixelSize + (asideWidth / 2) - textWidth / 2) / 2, 50), env);
    Draw.popMatrix(env);
}

let draw = (state, env) => {
  if (state.gameState == Gameover && Env.keyPressed(Space, env)) {
    getInitialState(env);
  } else if (state.gameState == Gameover) {
    let textWidth = Draw.textWidth(~font=state.font, ~body="Game Over", env);
    let subTextWidth = Draw.textWidth(~font=state.font, ~body="Press Space to restart", env);
    Draw.text(~font=state.font, ~body="Game Over", ~pos=(matrixWidth * pixelSize / 2 - textWidth / 2, matrixHeight * pixelSize / 2 - 20), env);
    Draw.text(~font=state.font, ~body="Press Space to restart", ~pos=(matrixWidth * pixelSize / 2 - subTextWidth / 2, matrixHeight * pixelSize / 2 + 20), env);
    state;
  } else {
    Draw.background(backgroundColor, env);
    drawShape(~relative=state.player, ~shape=state.shape, env);
    drawMatrix(state, env);

    let (score, matrix) = clearFullLines(~matrix=state.matrix);

    let isMoving = state.elapsed > delay;
    let keyUp = Env.keyPressed(Up, env);
    let keyLeft = Env.keyPressed(Left, env);
    let keyRight = Env.keyPressed(Right, env);
    let keyDown = state.keyDown ? !Env.keyReleased(Down, env) && !Env.keyPressed(Down, env): Env.keyPressed(Down, env);

    let (playerX, playerY) = state.player;
    let nextPlayerX = 
      switch (keyLeft, keyRight) {
      | (true, _) => minX(playerX - 1)
      | (_, true) => maxX(playerX + 1, state.shape)
      | (_, _) => playerX
      };
    let nextPlayerY = keyDown || isMoving ? playerY + 1 : playerY;
    let nextShapeMatrix = keyUp ? rotate(state.shape) : state.shape;
    let playerXDeltaAfterRotation = keyUp ? adjustPlayerAfterRotation(nextShapeMatrix, state.player) : 0;
    let nextPlayer = (nextPlayerX - playerXDeltaAfterRotation, nextPlayerY);
    let isColapsing =
      isColapsing(~player=nextPlayer, ~shape=state.shape, ~matrix);


    Draw.fill(asideColor, env);
    drawSideBar(state, env);

    if (isColapsing) {
      {
        ...state,
        matrix:
          setShapeInMatrix(
            ~shape=state.shape,
            ~player=nextPlayer,
            ~matrix,
          ),
        gameState: playerY <= 0 ? Gameover : state.gameState,
        shape: state.nextShape,  
        nextShape: getRandomShape(),
        player: (8, 0),
        score: state.score + score,
        keyDown: false,
        keyLeft: false,
        keyRight: false,
        keyUp: false
      };
    } else {
      {
        ...state,
        keyDown,
        keyUp,
        keyLeft,
        keyRight,
        matrix,
        score: state.score + score,
        player: nextPlayer,
        shape: nextShapeMatrix,
        elapsed: isMoving ? 0 : state.elapsed + 1,
      };
    };
  }
};

run(~setup, ~draw, ());
