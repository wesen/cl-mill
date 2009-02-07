package ruinwesen.lisp;

import java.awt.*;
import java.util.*;
import java.io.*;
import processing.core.*;

public class LispRecorder extends PGraphics {

    FileOutputStream fout;
    PrintStream pout;
    
    public void beginDraw() {
	try {
	    fout = new FileOutputStream(this.path);
	    pout = new PrintStream(fout);
	    pout.println("(with-p5-sketch ()");
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public void endDraw() {
	try {
	    pout.println(")");
	    fout.close();
	} catch (Exception e) {
	    e.printStackTrace();
	}
    }

    public void dispose() {
    }

    public void flush() {
    }

    public void hint(int which) {
    }

    public void beginShape() {
    }

    public void beginShape(int kind) {
    }

    public void edge(boolean edge) {
    }

    public void normal(float nx, float ny, float nz) {
    }

    public void textureMode(int mode) {
    }

    public void texture(PImage image) {
    }

    public void vertex(float x, float y) {
    }

    public void vertex(float x, float y, float z) {
    }

    public void vertex(float[] v) {
    }

    public void vertex(float x, float y, float u, float v) {
    }

    public void vertex(float x, float y, float z, float u, float v) {
    }

    public void breakShape() {
    }

    public void endShape() {
    }

    public void endShape(int mode) {
    }

    public void bezierVertex(float x2, float y2, float x3, float y3, float x4, float y4) {
    }

    public void bezierVertex(float x2, float y2, float z2,
			     float x3, float y3, float z3,
			     float x4, float y4, float z4) {
    }

    public void curveVertex(float x, float y) {
    }

    public void curveVertex(float x, float y, float z) {
    }

    public void point(float x, float y) {
	pout.println("(p5-point :x " + x + " :y " + y + ")");
    }

    public void point(float x, float y, float z) {
	pout.println("(p5-point :x " + x + " :y " + y + " :z " + z + ")");
    }

    public void line(float x1, float y1, float x2, float y2) {
	pout.println("(p5-line :x1 " + x1 + " :y1 " + y1 + " :x2 " + x2 + " :y2 " + y2 + ")");
    }

    public void line(float x1, float y1, float z1,
		     float x2, float y2, float z2) {
	pout.println("(p5-line :x1 " + x1 + " :y1 " + y1 + " :z1 " + z1 + " :x2 " +
		     x2 + " :y2 " + y2 + " :z2 " + z2 + ")");
    }

    public void triangle(float x1, float y1, float x2, float y2, float x3, float y3) {
	pout.println("(p5-triangle :x1 " + x1 + " :y1 " + y1 + " :x2 " + x2 + " :y2 " + y2
		     + " :x3 " + x3 + " :y3 " + y3 + ")");
    }

    public void quad(float x1, float y1, float x2, float y2,
		     float x3, float y3, float x4, float y4) {
	pout.println("(p5-quad :x1 " + x1 + " :y1 " + y1 + " :x2 " + x2 + " :y2 " + y2
		     + " :x3 " + x3 + " :y3 " + y3 + " :x4 " + x4 + " :y4 " + y4 + ")");
    }

    public void rectMode(int mode) {
    }

    public void rect(float a, float b, float c, float d) {
	pout.println("(p5-rect :x " + a + " :y " + a + " :width " + c + " :height " + d + ")");
    }

    public void ellipseMode(int mode) {
    }

    public void ellipse(float a, float b, float c, float d) {
	pout.println("(p5-ellipse :x " + a + " :y " + b + " :width " + c + " :height " + d + ")");
    }

    public void arc(float a, float b, float c, float d,
		    float start, float stop) {
	pout.println("(p5-arc :a " + a + " :b " + b + " :c " + c + " :d " + d + " :start " +
		     start + " :stop " + stop + ")");
    }

    public void box(float size) {
    }

    public void box(float w, float h, float d) {
    }

    public void sphereDetail(int res) {
    }

    public void sphereDetail(int ures, int vres) {
    }

    public void sphere(float r) {
    }

    public void bezierDetail(int detail) {
    }

    public void bezier(float x1, float y1, float x2, float y2,
		       float x3, float y3, float x4, float y4) {
	pout.println("(p5-bezier :x1 " + x1 + " :y1 " + y1 +
		     " :x2 " + x2 + " :y2 " + y2 +
		     " :x3 " + x3 + " :y3 " + y3 +
		     " :x4 " + x4 + " :y4 " + y4 + ")");
    }

    public void bezier(float x1, float y1, float z1,
		       float x2, float y2, float z2,
		       float x3, float y3, float z3,
		       float x4, float y4, float z4) {
    }

    public void curveDetail(int detail) {
    }

    public void curveTightness(float tightness) {
    }

    public void curve(float x1, float y1, float x2, float y2,
		      float x3, float y3, float x4, float y4) {
    }

    public void curve(float x1, float y1, float z1,
		      float x2, float y2, float z2,
		      float x3, float y3, float z3,
		      float x4, float y4, float z4) {
    }

    public void smooth() {
    }

    public void noSmooth() {
    }

    public void imageMode(int mode) {
    }

    public void image(PImage image, float x, float y) {
    }

    public void image(PImage image, float x, float y, float c, float d) {
    }

    public void image(PImage image, float a, float b, float c, float d,
		      int u1, int v1, int u2, int v2) {
    }

    public void shapeMode(int mode) {
    }

    public void shape(PShape shape) {
    }

    public void shape(PShape shape, float x, float y) {
    }

    public void shape(PShape shape, float x, float y, float c, float d) {
    }

    public void textAlign(int align) {
    }

    public void textAlign(int alignX, int alignY) {
    }

    public void textFont(PFont which) {
    }

    public void textFont(PFont which, float size) {
    }

    public void textLeading(float leading) {
    }

    public void textMode(int mode) {
    }

    public void textSize(float size) {
    }

    public void text(char c) {
    }

    public void text(char c, float x, float y) {
    }

    public void text(char c, float x, float y, float z) {
    }

    public void text(String str) {
    }

    public void text(String str, float x, float y) {
    }

    public void text(String str, float x, float y, float z) {
    }

    public void text(String str, float x1, float y1, float x2, float y2) {
    }

    public void text(String str, float x1, float y1, float x2, float y2, float z) {
    }

    public void text(int num, float x, float y) {
    }

    public void text(int num, float x, float y, float z) {
    }

    public void text(float num, float x, float y) {
    }

    public void text(float num, float x, float y, float z) {
    }

    public void pushMatrix() {
	pout.println("(p5-push-matrix)");
    }

    public void popMatrix() {
	pout.println("(p5-pop-matrix)");
    }

    public void translate(float tx, float ty) {
	pout.println("(p5-translate :tx " + tx + " :ty " + ty + ")");
    }

    public void translate(float tx, float ty, float tz) {
	pout.println("(p5-translate :tx " + tx + " :ty " + ty + " :tz " + tz + ")");
    }

    public void rotate(float angle) {
	pout.println("(p5-rotate :angle " + angle + ")");
    }

    public void rotateX(float angle) {
	pout.println("(p5-rotate-x :angle " + angle + ")");
    }

    public void rotateY(float angle) {
	pout.println("(p5-rotate-y :angle " + angle + ")");
    }

    public void rotateZ(float angle) {
	pout.println("(p5-rotate-z :angle " + angle + ")");
    }

    public void rotate(float angle, float vx, float vy, float vz) {
	pout.println("(p5-rotate :angle " + angle + " :vx " + vx +
		     " :vy " + vy + " :vz " + vz + ")");
    }

    public void scale(float s) {
	pout.println("(p5-scale :s " + s + ")");
    }

    public void scale(float sx, float sy) {
	pout.println("(p5-scale :sx " + sx + " :sy " + sy + ")");
    }

    public void scale(float x, float y, float z) {
	pout.println("(p5-scale :sx " + x + " :sy " + y + " :sz " + z + ")");
    }

    public void resetMatrix() {
	pout.println("(p5-reset-matrix)");
    }

    public void applyMatrix(PMatrix source) {
    }

    public void applyMatrix(PMatrix2D source) {
    }

    public void applyMatrix(float n00, float n01, float n02,
			    float n10, float n11, float n12) {
    }

    public void applyMatrix(PMatrix3D source) {
    }

    public void applyMatrix(float n00, float n01, float n02, float n03,
			    float n10, float n11, float n12, float n13,
			    float n20, float n21, float n22, float n23,
			    float n30, float n31, float n32, float n33) {
    }

    public void setMatrix(PMatrix source) {
    }

    public void setMatrix(PMatrix2D source) {
    }

    public void setMatrix(PMatrix3D source) {
    }

    public void printMatrix() {
    }


    public void beginCamera() {
    }

    public void endCamera() {
    }

    public void camera() {
    }

    public void camera(float eyeX, float eyeY, float eyeZ,
		       float centerX, float centerY, float centerZ,
		       float upX, float upY, float upZ) {
    }

    public void printCamera() {
    }

    public void ortho() {
    }

    public void ortho(float left, float right, float bottom, float top, float near, float far) {
    }

    public void perspective() {
    }

    public void perspective(float fovy, float aspect, float zNear, float zFar) {
    }

    public void frustum(float left, float right, float bottom,
			float top, float near, float far) {
    }

    public void printProjection() {
    }

    public void pushStyle() {
    }

    public void popStyle() {
    }

    public void style(PStyle s) {
    }

    public void strokeWeight(float weight) {
    }

    public void strokeJoin(int join) {
    }

    public void strokeCap(int cap) {
    }

    public void noStroke() {
    }

    public void stroke(int rgb) {
    }

    public void stroke(int rgb, float alpha) {
    }

    public void stroke(float gray) {
    }

    public void stroke(float gray, float alpha) {
    }

    public void stroke(float x, float y, float z) {
    }

    public void stroke(float x, float y, float z, float a) {
    }

    public void noTint() {
    }

    public void tint(int rgb) {
    }

    public void tint(int rgb, float alpha) {
    }

    public void tint(float gray) {
    }

    public void tint(float gray, float alpha) {
    }

    public void tint(float x, float y, float z) {
    }

    public void tint(float x, float y, float z, float a) {
    }

    public void noFill() {
    }

    public void fill(int rgb) {
    }

    public void fill(int rgb, float alpha) {
    }

    public void fill(float gray) {
    }

    public void fill(float gray, float alpha) {
    }

    public void fill(float x, float y, float z) {
    }

    public void fill(float x, float y, float z, float a) {
    }

    public void ambient(int rgb) {
    }

    public void ambient(float gray) {
    }

    public void ambient(float x, float y, float z) {
    }

    public void specular(int rgb) {
    }

    public void specular(float gray) {
    }

    public void specular(float x, float y, float z) {
    }

    public void hininess(float shine) {
    }

    public void emissive(int rgb) {
    }

    public void emissive(float gray) {
    }

    public void emissive(float x, float y, float z) {
    }

    public void lights() {
    }

    public void noLights() {
    }

    public void ambientLight(float red, float green, float blue) {
    }

    public void ambientLight(float red, float green, float blue,
			     float x, float y, float z) {
    }

    public void directionalLight(float red, float green, float blue,
				 float nx, float ny, float nz) {
    }

    public void pointLight(float red, float green, float blue,
			   float x, float y, float z) {
    }

    public void lightFalloff(float constant, float linear, float quadratic) {
    }

    public void lightSpecular(float x, float y, float z) {
    }

    public void background(int rgb) {
    }

    public void background(int rgb, float alpha) {
    }

    public void background(float gray) {
    }

    public void background(float gray, float alpha) {
    }

    public void background(float x, float y, float z) {
    }
    
    public void background(float x, float y, float z, float a) {
    }

    public void background(PImage image) {
    }

    public void colorMode(int mode) {
    }

    public void colorMode(int mode, float max) {
    }

    public void colorMode(int mode, float maxX, float maxY, float maxZ) {
    }

    public void colorMode(int mode, float maxX, float maxY, float maxZ, float maxA) {
    }

    public void setCache(Object parent, Object storage) {
    }

    public void removeCache(Object parent) {
    }

    public void set(int x, int y,  int c) {
    }

    public void set(int x, int y, PImage src) {
    }

    public void mask(int alpha[]) {
    }

    public void mask(PImage alpha) {
    }

    public void filter(int kind) {
    }

    public void filter(int kind, float param) {
    }

    public void copy(int sx, int sy, int sw, int sh,
		     int dx, int dy, int dw, int dh) {
    }

    public void copy(PImage src,
		     int sx, int sy, int sw, int sh,
		     int dx, int dy, int dw, int dh) {
    }

    public void blend(int sx, int sy, int sw, int sh,
		      int dx, int dy, int dw, int dh, int mode) {
    }

    public void blend(PImage src,
		      int sx, int sy, int sw, int sh,
		      int dx, int dy, int dw, int dh, int mode) {
    }
}
