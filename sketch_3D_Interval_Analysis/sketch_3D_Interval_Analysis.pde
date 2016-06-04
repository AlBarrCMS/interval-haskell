import peasy.*;

PeasyCam cam;

Table cells;
int[] color_offset;
float z_scale;


void setup() {
  size(800, 800, P3D); 
  cam = new PeasyCam(this, 1000);
  cam.setMinimumDistance(0.00001);
  cam.setMaximumDistance(1000);
  
  cells = loadTable("paper.csv");
  color_offset = new int[cells.getRowCount()];
  float abs_z_max = 0;
  for (int i = 0; i < color_offset.length; i++) {
    color_offset[i] = (int)random(30)-15; 
    float z_min = cells.getRow(i).getFloat(0);
    float z_max = cells.getRow(i).getFloat(1);
    if (abs(z_min) > abs_z_max)
      abs_z_max = abs(z_min);
    if (abs(z_max) > abs_z_max)
      abs_z_max = abs(z_max);
  }
  z_scale = 200 / abs_z_max;
  strokeWeight(0.05);
  //noStroke();
  //println(cells.getRowCount());
  
}

void draw() {
  background(200);
   scale(40, -40, 40);
   println(frameRate);
   for (int i = 0; i < cells.getRowCount(); i++) {
    TableRow cell = cells.getRow(i);
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
      fill(100 + color_offset[i] + 30 * max);
      stroke(0);
    } else {
       fill(180 + color_offset[i] + 30 * max); 
       stroke(0);
    }
    
    //noStroke();
     
    min = sign(min) * log(abs(min) + 1);
    max = sign(max) * log(abs(max) + 1);
    draw_box(x1, y1, min, x2-x1, y2-y1, max-min);
  }

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