import peasy.*;

PeasyCam cam;

Table[] cells;
int[][] color_offset;
float z_scale;
String[] filenames = { "my_poly_rin.csv" };
int current_model = 0;


void setup() {
  size(800, 800, P3D);
  cam = new PeasyCam(this, 1000);
  cam.setMinimumDistance(0.00001);
  cam.setMaximumDistance(1000);

  /*cells = loadTable("paper.csv");*/
  cells = new Table[filenames.length];
  color_offset = new int[filenames.length][];
  float abs_z_max = 0;
  for (int i = 0; i < filenames.length; i++) {
    cells[i] = loadTable(filenames[i]);
    color_offset[i] = new int[cells[i].getRowCount()];
    println(cells[i].getRowCount(), cells[i].getColumnCount());
    for (int j = 0; j < color_offset.length; j++) {
      color_offset[i][j] = (int)random(30)-15;
      float z_min = cells[i].getRow(j).getFloat(0);
      float z_max = cells[i].getRow(j).getFloat(1);
      if (abs(z_min) > abs_z_max)
        abs_z_max = abs(z_min);
      if (abs(z_max) > abs_z_max)
        abs_z_max = abs(z_max);
    }
  }
  z_scale = 200 / abs_z_max;
  strokeWeight(0.05);
}

void draw() {
  background(200);
   scale(40, -40, 40);
   /*println(frameRate);*/
   for (int i = 0; i < cells[current_model].getRowCount(); i++) {
    TableRow cell = cells[current_model].getRow(i);
    float min = cell.getFloat(0) * z_scale;
    float max = cell.getFloat(1) * z_scale;
    float x1 = cell.getFloat(2);
    float x2 = cell.getFloat(3);
    float y1 = cell.getFloat(4);
    float y2 = cell.getFloat(5);

    if (min <= 0 && 0 <= max) {
      fill(255, 0, 0);
      stroke(255, 0, 0);
    } else if (max < 0) {
      fill(100 + color_offset[current_model][i] + 30 * max);
      stroke(0);
    } else {
       fill(180 + color_offset[current_model][i] + 30 * max);
       stroke(0);
    }

    //noStroke();

    min = sign(min) * log(abs(min) + 1);
    max = sign(max) * log(abs(max) + 1);
    draw_box(x1, y1, min, x2-x1, y2-y1, max-min);
  }

  //-----------Stopping peasy ------
  cam.beginHUD();
  fill(0, 100);
  rect(10, 10, 290, 70);
  fill(-1);
  textSize(26);
  text(filenames[current_model], 50, 50);
  cam.endHUD();
  //--------------------------------

}
int sign(float x) {
  if (x < 0) {
    return -1;
  } else if (x > 0) {
    return 1;
  } else {
    return 0;
  }
}

void draw_box(float x, float y, float z, float x_dim, float y_dim, float z_dim) {
  pushMatrix();
    translate(x + x_dim/2, y + y_dim/2, z + z_dim/2);
    box(x_dim, y_dim, z_dim);
  popMatrix();
}

void keyPressed() {
  if (key == ' ')
    current_model = (current_model + 1) % cells.length;
}
