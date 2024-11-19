import { getPostFilePaths, getCompiledMDX } from "@/utils/mdxUtils";
import { notFound } from "next/navigation";
import { Metadata } from "next";
import { getLatestPostSummaries } from "@/utils/mdxUtils";

export const generateStaticParams = async () => {
    const postFilePaths = await getPostFilePaths();

    return postFilePaths.map((path) => ({
        slug: path.replace(/.mdx?$/, "")
    }))
}


const getPostData = async (postSlug: string) => {
    try {
        return await getCompiledMDX(postSlug);
    } catch (e) {
        notFound();
    }
}

type PageProps = {
    params: {slug: string};
};


const BlogPostPage = async ({params}: PageProps) => {
    const {content, frontmatter} = await getPostData(params.slug);
    const {title, date} = frontmatter;
    return (
        <>
            <h1>{title}</h1>
            <p>{new Date(date).toDateString()}</p>
            {content}
        </>
    )
}

export const generateMetadata = async ({params}: PageProps): Promise<Metadata> => {
    try {
        const {frontmatter} = await getCompiledMDX(params.slug);
        return {
            title: frontmatter.title,
            description: frontmatter.description,
            openGraph: {
                title: frontmatter.title,
                description: frontmatter.description,
                type: "article", 
                url: `/blog/${params.slug}`,
            }
        }
    } catch (e) {
        notFound();
    }
}

export default BlogPostPage;

