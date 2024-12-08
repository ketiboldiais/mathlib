import fsp from "fs/promises"
import path from "path"
import { cache } from "react"
import { compileMDX } from "next-mdx-remote/rsc";
import remarkMath from "remark-math";
import remarkGfm from "remark-gfm";
import rehypeKatex from "rehype-katex";
import rehypeHighlight from "rehype-highlight";
import rehypeSlug from "rehype-slug";
import Terminal from "@/components/Terminal";
import REPL from "@/components/REPL";

export type PostsFrontMatter = {
    title: string;
    description: string;
    date: string;
}

export const getCompiledMDX = cache(async (postSlug: string) => {
    const postFilePath = path.join(POSTS_PATH, `${postSlug}.mdx`); 
    const source = await fsp.readFile(postFilePath);
    return compileMDX<PostsFrontMatter>({
        source, 
        components: {
            Terminal: Terminal,
            REPL: REPL,
        },
        options: {
            mdxOptions: {
                rehypePlugins: [rehypeKatex, rehypeHighlight, rehypeSlug],
                remarkPlugins: [remarkGfm, remarkMath],
            },
            parseFrontmatter: true,
        }
    })
})

const POSTS_PATH = path.join(process.cwd(), "pages");

export const getPostFilePaths = cache(async ()=> {
    const dirFiles = await fsp.readdir(POSTS_PATH);

    return dirFiles.filter((filepath) => /.mdx?$/.test(filepath));
})

export const getLatestPostSummaries = cache(async () => {
    const filePaths = await getPostFilePaths();
    const posts = await Promise.all(
        filePaths.map(async (filepath) => {
            const slug = filepath.replace(/.mdx?$/, "");
            return {slug, ...(await getCompiledMDX(slug))};
        })
    );
    posts.sort((a,b) => (a.frontmatter.date <b.frontmatter.date ? 1 : -1));

    return posts.map((post) => ({slug: post.slug, ...post.frontmatter}));
})

export const getPostsSitemap = cache(async () => {
    const filePaths = await getPostFilePaths();
    return await Promise.all(
        filePaths.map(async (filepath) => {
            const slug = filepath.replace(/.mdx?$/, "");
            return { slug, ...(await fsp.stat(path.join(POSTS_PATH, filepath)))}
        })
    )
})