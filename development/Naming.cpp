#include <vector>
#include <string>
#include <unordered_map>
#include <iostream>

class NamedVector {
private:
    std::vector<double> values;
    std::unordered_map<std::string, size_t> name_to_index;

public:
    // Add an element with a name
    void add_element(const std::string& name, double value) {
        values.push_back(value);
        name_to_index[name] = values.size() - 1;
    }

    // Get element by name
    double get_element(const std::string& name) const {
        auto it = name_to_index.find(name);
        if (it != name_to_index.end()) {
            return values[it->second];
        }
        throw std::runtime_error("Name not found");
    }

    // Display all elements and their names
    void display() const {
        for (const auto& pair : name_to_index) {
            std::cout << pair.first << ": " << values[pair.second] << std::endl;
        }
    }
};

int main() {
    NamedVector nv;
    nv.add_element("a", 1.0);
    nv.add_element("b", 2.0);
    nv.add_element("c", 3.0);

    std::cout << "Element 'b': " << nv.get_element("b") << std::endl;

    std::cout << "All elements:" << std::endl;
    nv.display();

    return 0;
}

