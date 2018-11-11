open Reprocessing;

type pixelT =
  | Empty
  | Fill;

type shapeT = {
  matrix: list(list(pixelT)),
  color: colorT,
};

type stateT = {
  player: (int, int),
  matrix: array(array(pixelT)),
  shape: shapeT,
  keyUp: bool,
  keyDown: bool,
  keyLeft: bool,
  keyRight: bool,
  elapsed: int,
};

let square = [[Fill, Fill], [Fill, Fill]];
let line = [[Fill, Fill, Fill, Fill]];
let tShape = [[Fill, Fill, Fill], [Empty, Fill, Empty]];
let lShape = [[Fill, Fill, Fill, Empty], [Fill, Empty, Empty, Empty]];
let zShape = [[Fill, Fill, Empty], [Empty, Fill, Fill]];

let shapes = [square, line, tShape, lShape, zShape];
let colors = [
  Utils.color(~r=26, ~g=173, ~b=158, ~a=255),
  Utils.color(~r=232, ~g=232, ~b=232, ~a=255),
  Utils.color(~r=229, ~g=108, ~b=94, ~a=255),
];

let backgroundColor = Utils.color(~r=42, ~g=42, ~b=42, ~a=255);
let getRandomShape = () => {
  matrix: List.nth(shapes, Random.int(List.length(shapes))),
  color: List.nth(colors, Random.int(List.length(colors))),
};

let matrixHeight = 30;
let matrixWidth = 20;
let pixelSize = 30;

let delay = 20;

let setup = env => {
  Env.size(
    ~width=matrixWidth * pixelSize,
    ~height=matrixHeight * pixelSize,
    env,
  );
  {
    player: (8, 2),
    shape: getRandomShape(),
    matrix: Array.make_matrix(matrixHeight, matrixWidth, Empty),
    keyUp: false,
    keyDown: false,
    keyLeft: false,
    keyRight: false,
    elapsed: 0,
  };
};

let drawMatrix = ({matrix}, env) => {
  Array.iteri(
    (y, row) =>
    Array.iteri(
        (x, pixel) => {
          Draw.fill(colors -> List.hd, env);
          if (pixel == Fill) {
            Draw.rect(
              ~pos=(x * pixelSize, y * pixelSize),
              ~width=pixelSize,
              ~height=pixelSize,
              env,
            );
          };
        },
        row,
      ),
    matrix,
  );
}

let drawShape = ({shape, player}, env) =>
  List.iteri(
    (y, row) =>
      List.iteri(
        (x, pixel) => {
          Draw.fill(shape.color, env);
          if (pixel == Fill) {
            let (playerX, playerY) = player;
            let x = playerX + x;
            let y = playerY + y;

            Draw.rect(
              ~pos=(x * pixelSize, y * pixelSize),
              ~width=pixelSize,
              ~height=pixelSize,
              env,
            );
          };
        },
        row,
      ),
    shape.matrix,
  );

let flip = matrix =>
  List.mapi(
    (i, _column) => List.map(row => List.nth(row, i), matrix),
    List.hd(matrix),
  );

let rotate = matrix => flip(List.rev(matrix));

let maxX = x => x > matrixWidth ? matrixWidth : x;
let minX = x => x < 0 ? 0 : x;
let maxY = y => y > matrixHeight ? matrixHeight : y;

let isColapsing = (~player, ~shape: shapeT, ~matrix) => {
  let found = ref(false);

  List.iteri(
    (y, row) =>
      List.iteri(
        (x, pixel) =>
          if (pixel == Fill) {
            let (playerX, playerY) = player;
            let x = playerX + x;
            let y = playerY + y;
            
            if (y >= matrixHeight - 1 || matrix[y + 1][x] == Fill) {
              found := true;
            };
          },
        row,
      ),
    shape.matrix,
  );

  found^;
};

let setShapeInMatrix = (~shape: shapeT, ~player, ~matrix) =>
  Array.mapi(
    (y, row) =>
      Array.mapi(
        (x, pixel) => {
          let (playerX, playerY) = player;
          
          if (x >= playerX
              && y >= playerY
              && y - playerY <= shape.matrix -> List.length - 1
              && x - playerX <= shape.matrix -> List.hd -> List.length - 1
              && shape.matrix
                 -> List.nth(y - playerY)
                 -> List.nth(x - playerX)
              == Fill) {
            Fill;
          } else {
            pixel;
          };
        },
        row,
      ),
    matrix,
  );

let draw = (state, env) => {
  Draw.background(backgroundColor, env);
  drawShape(state, env);
  drawMatrix(state, env);

  let isMoving = state.elapsed > delay;
  let keyUp = Env.keyPressed(Up, env);
  let keyLeft = Env.keyPressed(Left, env);
  let keyRight = Env.keyPressed(Right, env);
  let keyDown = Env.keyPressed(Down, env);

  let (playerX, playerY) = state.player;

  let nextPlayerX =
    switch (keyLeft, keyRight) {
    | (true, _) => minX(playerX - 1)
    | (_, true) => maxX(playerX + 1)
    | (_, _) => playerX
    };
  let nextPlayerY = keyDown || isMoving ? playerY + 1 : playerY;
  let nextShapeMatrix =
    keyUp ? rotate(state.shape.matrix) : state.shape.matrix;
  let nextPlayer = (nextPlayerX, nextPlayerY);
  let isColapsing = isColapsing(~player=nextPlayer, ~shape=state.shape, ~matrix=state.matrix);

  if (isColapsing) {
    {...state, matrix: setShapeInMatrix(~shape=state.shape, ~player=nextPlayer, ~matrix=state.matrix), 
      shape: getRandomShape(), player: (8, 2)};
  } else {
    {
      ...state,
      keyDown,
      keyUp,
      keyLeft,
      keyRight,
      player: nextPlayer,
      shape: {
        ...state.shape,
        matrix: nextShapeMatrix,
      },
      elapsed: isMoving ? 0 : state.elapsed + 1,
    };
  };
};

run(~setup, ~draw, ());
