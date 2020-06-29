import pathlib

from setuptools import setup

# The directory containing this file
HERE = pathlib.Path(__file__).parent

# The text of the README file
README = (HERE / "README.md").read_text()

# This call to setup() does all the work
setup(
    name="sgx",
    version="1.5.0",
    description="Wrapper for unofficial SGX API",
    long_description=README,
    long_description_content_type="text/markdown",
    url="https://github.com/hueyy/pysgx",
    author="Huey",
    author_email="hello@huey.xyz",
    license="UNLICENSED",
    classifiers=[
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
    ],
    packages=["sgx"],
    include_package_data=True,
    install_requires=["requests", "xmltodict"]
)
