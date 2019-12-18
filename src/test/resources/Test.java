import java.io.File;
import java.io.FileReader;
import java.io.IOException;

class Test {
    void main(File f) throws IOException {
        FileReader fileReader = new FileReader(f);
        fileReader.close();
        f.delete();
    }
}